#!/bin/bash

# This file is part of Marionnet, a virtual network laboratory
# Copyright (C) 2013  Jean vincent Loddo
# Copyright (C) 2013  Université Paris 13
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This script helps people to build a debian filesystem
# with debootstrap according to the Marionnet requirements.

set -e

if [[ $1 = wheezy || $1 = squeeze ]]; then
 RELEASE="$1"
 echo "Release set to \`$RELEASE'"
else
 echo "Usage: $(basename $0) RELEASE"
 echo "where RELEASE is \`wheezy' or \`squeeze'"
 echo "---"
 echo "Generates the file \`package_catalog.\${RELEASE}.GENERATED' from \`binary_list.UNION'"
 echo "searching in the specified RELEASE for packages related to at least one of the listed"
 echo "binaries."
 exit 2
fi

HTTP_SERVER=http://ftp.debian.org/debian/
ARCH=i386

function mkTMPFILE {
 mktemp /tmp/${1}.$(date +%H\h%M | tr -d " ").XXXXXX
}

# 1-column file difference:
function list_diff {
 [[ -f "$1" && -f "$2" ]] || { return 1; }
 local PATTERNS=$(sort "$2" | uniq)
 sort "$1" | uniq | \grep -v -w -F "$PATTERNS"
}

# rewrite [-f/--follow] FILE with COMMAND
# Examples:
# $ rewrite FOO with grep "DATE=" FOO
# $ rewrite FOO with grep "DATE=" <FOO
function rewrite { 
 local FOLLOW
 if [[ "$1" = "-f" || "$1" = "--follow" ]]; then
   FOLLOW=y
   shift
 fi
 [[ $# -ge 3 && -w "$1" && "$2" = "with" ]] || { 
   echo "Usage: rewrite FILE with COMMAND"
   echo "where COMMAND refers to FILE (as argument or standard input)"
   return 2
   }
 local TARGET="$1"
 shift 2
 local TMPFILE=$(mktemp)
 if [[ -z $FOLLOW ]]; then
   "$@" > $TMPFILE
 else
   "$@" | tee $TMPFILE
 fi
 cat $TMPFILE > $TARGET
 rm -f $TMPFILE
}

function cool_sudo_chroot { 
 # global COOL_SUDO
 if [[ -z $COOL_SUDO ]]; then
   export COOL_SUDO=$(mktemp /tmp/COOL_SUDO.XXXXXX)
   chmod +x $COOL_SUDO
 fi
 local DIR="$1"
 shift
 if [[ $# -eq 0 ]]; then
   set -- "bash" "2>/dev/null"
 fi
 echo '#!/bin/bash'         > $COOL_SUDO
 echo "chroot '$DIR'" "$@" >> $COOL_SUDO
 sudo $COOL_SUDO
}

function cool_sudo { 
 # global COOL_SUDO
 if [[ -z $COOL_SUDO ]]; then
   export COOL_SUDO=$(mktemp /tmp/COOL_SUDO.XXXXXX)
   chmod +x $COOL_SUDO
 fi
 if [[ $# -eq 0 ]]; then
   set -- "bash" "2>/dev/null"
 fi
 echo '#!/bin/bash'  > $COOL_SUDO
 echo "$@"          >> $COOL_SUDO
 sudo $COOL_SUDO
}

function make_package_catalog_from_binary_list {
 # global RELEASE HTTP_SERVER ARCH
 # Required file: binary_list.UNION
 local REQUIRED_FILE=binary_list.UNION
 local TARGET=package_catalog.$RELEASE.GENERATED
 # ---
 [[ -f $REQUIRED_FILE ]] || {
   make -C $(dirname $REQUIRED_FILE) $(basename $REQUIRED_FILE)
   }
 local TWDIR=_build.temporary_basic_${RELEASE}.$(date +%Y-%m-%d.%H\h%M)
 local INCLUDE="--include=realpath,apt-file"
 local EXCLUDE="--exclude=udev"
 mkdir -v -p $TWDIR
 # ---
 echo "Step 1: making a temporary debian system"
 echo "---"
 cool_sudo debootstrap --arch=${ARCH} $INCLUDE $EXCLUDE $RELEASE ${TWDIR} ${HTTP_SERVER}
 # ---
 local BINARY_LIST=$(<$REQUIRED_FILE)
 local TARGET1=$(mkTMPFILE TARGET1)
 local TARGET2=$(mkTMPFILE TARGET2)
 local TARGET3=$(mkTMPFILE TARGET3)
 local TARGET4=$(mkTMPFILE TARGET4)
 local TMPFILE1=$(mktemp)
 local TMPFILE2=$(mktemp)
 local TMPFILE3=$(mktemp)
 # ---
 echo "Step 2: searching for listed binaries which are not included in a basic debian ($RELEASE) system..."
 echo "---"
 local CMD=$(echo 'for i in '$BINARY_LIST'; do echo -en "$i\t" ; dpkg -S $(realpath $(type -P $i)) || echo NOT_FOUND; done')
 cool_sudo_chroot $TWDIR <<<"$CMD" 1>$TARGET1
 # ---
 echo "Step 3: searching for packages containing not basic binaries..."
 echo "---"
 local R
 CMD="apt-file update"
 sudo <<<"$CMD" chroot $TWDIR bash
 \grep NOT_FOUND $TARGET1 \
   | while read i _; do 
       CMD=$(echo "apt-file search bin/$i | grep 'bin/$i$' | head -n 1")
       R=$(cool_sudo_chroot $TWDIR <<<"$CMD")
       if [[ -n "$R" ]]; then echo -e "$i $R"; else echo -e "$i NOT_FOUND"; fi
     done \
   | tee $TMPFILE1
 awk <$TMPFILE1 '(NF == 3) {print $2,$1}' | uniq | sort -k 1,1 -d | uniq >$TMPFILE2
 PACKAGES=$(awk <$TMPFILE2 -F : '{print $1}' | uniq | sort | uniq)
 rewrite $TMPFILE2 with tr -d ':' <$TMPFILE2
 awk <$TMPFILE2 '{p1=$1; if (p1 != p0) printf("\n%s PROVIDES: %s",p1,$2); else printf(" %s",$2); p0=p1; next}' >$TARGET2
 # ---
 echo "Step 4: adding packages' descriptions..."
 echo "---"
 local L="en_US@UTF-8";
 local PACKAGE REST
 while read PACKAGE REST; do 
   CMD="LANGUAGE=$L LANG=$L LC_ALL=$L aptitude show $PACKAGE"
   cool_sudo_chroot $TWDIR <<<"$CMD" | awk -v PACKAGE="$PACKAGE" -v REST="$REST" '/^Description:/ {$1=""; print PACKAGE,REST,"DESCRIPTION:",$0}'; 
 done <$TARGET2 | tee $TARGET3
 # ---
 echo "Step 5: removing basic packages from catalog..."
 echo "---"
 awk <$TARGET3 '{print $1}' > $TMPFILE1 # list of packages currently listed
 sudo <<<"dpkg -l" chroot $TWDIR bash | awk '($1 == "ii") {print $2}' > $TMPFILE2 # list of basic packages to ignore
 list_diff $TMPFILE1 $TMPFILE2 > $TMPFILE3 # list of interesting packages
 join -j 1 $TMPFILE3 $TARGET3 > $TARGET4
 # Finish
 [[ -f $TARGET ]] && cp -v "$TARGET" "$TARGET.backup" --backup="numbered"
 cat $TARGET4 > $TARGET
 echo "Catalog generated in $TARGET"
 # Mrproper:
 rm -f $TARGET{1,2,3,4} $TMPFILE{1,2,3}
 local ANSWER
 echo -n "Do you want to remove \`$TWDIR'? (y/..) "; read ANSWER
 if [[ $ANSWER = y ]]; then
   cool_sudo rm -rf $TWDIR 
 fi
}

### Unsuccessfully tested with fakeroot (version 1.18.2-1) and fakechroot (version 2.16-1) :
### --------------------------------------------------------
# # # fakechroot -s fakeroot debootstrap --foreign --variant=fakechroot --include=fakechroot,fakeroot --arch=${ARCH} $RELEASE ${TWDIR} ${HTTP_SERVER}
# # # DEBOOTSTRAP_DIR=${TWDIR}/debootstrap fakechroot fakeroot debootstrap --second-stage --second-stage-target=${TWDIR}
# # # I: Installing core packages...
# # # W: Failure trying to run: chroot _build.debian-wheezy-with-linux-.2013-06-14.17h46 dpkg --force-depends --install /var/cache/apt/archives/base-files_7.1_i386.deb /var/cache/apt/archives/base-passwd_3.5.26_i386.deb
### --------------------------------------------------------

make_package_catalog_from_binary_list
echo "Success."
exit 0
