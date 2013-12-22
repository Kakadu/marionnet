#!/bin/bash

# This file is part of Marionnet
# Copyright (C) 2013  Jean-Vincent Loddo
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

# Preamble for automatic log-file generation:

MY_BASENAME=$(basename $0)
if [[ $1 = "--help" || $1 = "-h" || $1 = "--list" || $1 = "-l" || $1 = "--source" || $1 = "-s" ]]; then
  # do nothing and continue
  :
elif grep -q "log_${MY_BASENAME}[.]......$" <<<"$1"; then
  LOGFILE=$1
  shift
  # and continue
else
  LOGFILE=$(mktemp /tmp/log_${MY_BASENAME}.XXXXXX)
  EXIT_CODE_FILE=$(mktemp /tmp/exit_code_${MY_BASENAME}.XXXXXX)
  echo -e "Log file of command:\n$0" "$@" "\n---" >$LOGFILE
  COLUMNS=$(tput cols)
  { time $0 "$LOGFILE" "$@"; echo $? >$EXIT_CODE_FILE; } 2>&1 | tee -a "$LOGFILE" | cut -c1-$((COLUMNS))
  read EXIT_CODE <$EXIT_CODE_FILE
  rm -f $EXIT_CODE_FILE
  echo "---"
  echo "$MY_BASENAME: previous running logged into $LOGFILE"
  exit $EXIT_CODE
fi

[[ $1 = "--source" || $1 = "-s" ]] || {
set -e
shopt -s nullglob
shopt -s expand_aliases
}

# Getopt's format used to parse the command line:
OPTSTRING="hlsc"

function parse_cmdline {
local i j flag
# Transform long format options into the short one:
for i in "$@"; do
  if [[ double_dash_found = 1 ]]; then
    ARGS+=("$i")
  else case "$i" in
    --custom)
      ARGS+=("-c");
      ;;
    --help)
      ARGS+=("-h");
      ;;
    --source)
     ARGS+=("-s");
     ;;
    --list)
     ARGS+=("-l") ;;
    --)
      ARGS+=("--");
      double_dash_found=1;
      ;;
    --[a-zA-Z0-9]*)
      echo "*** Illegal long option $i.";
      exit 1;
      ;;
    -[a-zA-Z0-9]*)
      j="${i:1}";
      while [[ $j != "" ]]; do ARGS+=("-${j:0:1}"); j="${j:1}"; done;
      ;;
    *)
      ARGS+=("$i")
      ;;
  esac
  fi
done
set - "${ARGS[@]}"
unset ARGS

# Interpret short format options:
while [[ $# -gt 0 ]]; do
  OPTIND=1
  while getopts ":$OPTSTRING" flag; do
    if [[ $flag = '?' ]]; then
      echo "ERROR: illegal option -$OPTARG.";
      exit 1;
    fi
    eval "option_${flag}=$OPTIND"
    eval "option_${flag}_arg='$OPTARG'"
  done
  for ((j=1; j<OPTIND; j++)) do
    if [[ $1 = "--" ]]; then
      shift;
      for i in "$@"; do ARGS+=("$i"); shift; done
      break 2;
    else
      shift;
    fi
  done
  # Get just the first argument and reloop:
  for i in "$@"; do ARGS+=("$i"); shift; break; done
done
} # end of parse_cmdline()

declare -a ARGS
parse_cmdline "$@" # read OPTSTRING and set ARGS

# Warning: the following two branches could not be grouped
# into the single command (`else' branch):
# set - "${ARGS[@]}";
# The behaviour is not the same when the array is empty!
if [[ ${#ARGS[@]} -eq 0 ]]; then
  set - "";
  shift
else
  set - "${ARGS[@]}";
fi
unset ARGS

function print_usage_and_exit {
 echo "Download, patch and compile a kernel.
Usage: ${0##*/} [OPTIONS] <KERNEL_VERSION> [WORKING-DIRECTORY]
   or: ${0##*/} (--help|-h)
   or: ${0##*/} (--list|-l)
   or: source ${0##*/} (--source|-s)

The first synopsis builds a kernel.
The second synopsis prints this message and exits.
The third synopsis shown the list of defined functions.
The fourth synopsis allows this script to be sourced
(to have relevant functions available in the current environment).

Options:
  -c/--custom    customize the kernel using 'make menuconfig'

Example:
$ ${0##*/} 3.4.22
$ ${0##*/} 3.4.22 /tmp/_build.678HG234
$ ${0##*/} -l
$ source ${0##*/} -s"
 exit $1
}

# Manage now your options in a convenient order
#
# Option -h
if [[ -n ${option_h} ]]; then
 print_usage_and_exit 0
fi

AWK_PROGRAM_LISTING_FUNCTIONS='/^[ ]*function[ ]*[a-zA-Z0-9_]*[ ]*{/ && ($2 != "parse_cmdline") && ($2 != "print_usage_and_exit") {print $2}'

# Option -l/--list
if [[ -n ${option_l} ]]; then
  awk <"$0" "$AWK_PROGRAM_LISTING_FUNCTIONS" | sort
 exit 0
fi

# Option -c/--custom
if [[ -n ${option_c} ]]; then
  CUSTOM_OPTION="--custom"
fi

####################################
#           M  A  I  N             #
####################################

# -------------------
# Configuring kernels
# -------------------

# Sort and merge `.config' files removing comments and empty lines.
# Note that the operation of sorting lines provides the expected behaviour: the
# letter "y" (yes) comes after "n" (no), that comes after "m" (module).
# In other terms, if a variable X is set twice, for instance X=m in a file, and
# X=y in the other file, the resulting file will be made with the line "X=m" before
# the line "X=y". In this way, "make oldconfig" will take "X=y" discarding the previous
# setting, as we expect. In the case of "X=n" vs "X=m", "no" wins.
function sort_and_merge_config_files {
 cat "$@" | awk 'NF>0 && $1 !~ /^#/' | sort | uniq
}


# Usage: create_kernel_config_from [-i] <CONFIG-FILE>
#
# Example:
#   create_kernel_config_from CONFIG-2.6.18
#
# Successfully tested with 3.2.{13,44}, 3.4.42, 3.8.10
function create_kernel_config_from {
 local INTERACTIVE
 if [[ $1 = -i ]]; then INTERACTIVE=y; shift; fi
 local DEFAULT_OLD_CONFIG_FILE=$PWD/CONFIG-2.6.18
 local OLD_CONFIG_FILE=${1:-$DEFAULT_OLD_CONFIG_FILE}
 [[ -f $OLD_CONFIG_FILE ]] || return 1
 # Make a default .config for ARCH=um
 make mrproper
 make mrproper ARCH=um
 make defconfig ARCH=um
 # Merge with the provided (good) .config
 sort_and_merge_config_files .config $OLD_CONFIG_FILE >.config.1
 mv .config.1 .config
 if [[ $INTERACTIVE = y ]]; then
   make oldconfig ARCH=um SUBARCH=i386
 else
   while true; do echo; done | make oldconfig ARCH=um SUBARCH=i386
 fi
 # Finally fix some specific problems:
 # UML_NET_PCAP must be unset (error compiling the kernel):
 # (unhappily because in this way we cannot start wireshark as normal user,
 #  see http://wiki.wireshark.org/CaptureSetup/CapturePrivileges)
 sed -i -e 's/CONFIG_UML_NET_PCAP=y/CONFIG_UML_NET_PCAP=n/' .config
 # Looking linux-3.0.75/arch/x86/lib/Makefile this variable must be unset:
 sed -i -e 's/CONFIG_X86_CMPXCHG64=y/CONFIG_X86_CMPXCHG64=n/' .config
 # Looking http://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/commit/?id=084189a
 sed -i -e 's/CONFIG_CMPXCHG_LOCAL=y/CONFIG_CMPXCHG_LOCAL=n/' .config
 # The modern `systemd' mechanism requires DEVTMPFS:
 echo 'CONFIG_DEVTMPFS=y' >> .config
 echo 'CONFIG_DEVTMPFS_MOUNT=n' >> .config
 # Now switch to "yes" (=y) all remaining things set as module (=m).
 # In the case of kernel 3.2.44 (LTS) we have only this settings:
 # CONFIG_UML_SOUND=m
 # CONFIG_SOUND=m
 # CONFIG_HOSTAUDIO=m
 # CONFIG_HW_RANDOM=m
 # CONFIG_CRYPTO_RNG=m
 # CONFIG_CRYPTO_ANSI_CPRNG=m
 sed -i -e 's/=m/=y/' .config
 # A second time, in order to have a very clean .config:
 while true; do echo; done | make oldconfig ARCH=um SUBARCH=i386
 echo "Ok, result of merging and cleaning in \`.config'"
}


# ----------------
# Compilig kernels
# ----------------

# For instance, if we call this function when we are (PWD) in
# "/home/myrepos/marionnet/uml/pupisto/pupisto.sh.files/"
# the result will be "/home/myrepos/marionnet/uml/"
# This is useful to get files which location is known with
# respect to this path (for instance kernel patches).
function get_our_marionnet_slash_uml_directory_path {
 # Global PWD
 local TRAILER=${PWD##*/marionnet/uml/}
 echo ${PWD%$TRAILER}
}


# Usage:
# $ download_patch_and_compile_kernel [c/--custom] <KERNEL_VERSION> [WORKING-DIRECTORY]
#
# Example:
# $ download_patch_and_compile_kernel 3.2.48 /tmp/_building_directory
function download_patch_and_compile_kernel {
local CUSTOM
if [[ $1 = "-c" || $1 = "--custom" ]]; then
  CUSTOM=y
  shift
fi
# global CUSTOM
[[ $# -ge 1 ]] || return 1

# For instance "3.2.48"
local VERSION=$1
local TWDIR=${2:-.}
local DOWNLOADS_DIRECTORY=${3:-$PWD/_build.downloads}

# Before pushing, get our marionnet/uml/kernel directory:
local OUR_KERNEL_DIR=$(get_our_marionnet_slash_uml_directory_path)/kernel

# Move to the working temporary directory (or current directory):
pushd "$TWDIR"

# Download, uncompress and untar the kernel:
local KERNEL_SUBDIR=${VERSION%.*}
local KERNEL_SUBDIR=${KERNEL_SUBDIR//3.?/3.x}

# To save the tarball:
mkdir -p $DOWNLOADS_DIRECTORY

if [[ -f linux-${VERSION}.tar.xz ]]; then
  tar -xJf linux-${VERSION}.tar.xz
  mv linux-${VERSION}.tar.xz $DOWNLOADS_DIRECTORY/
elif [[ -f $DOWNLOADS_DIRECTORY/linux-${VERSION}.tar.xz ]]; then
  tar -xJf $DOWNLOADS_DIRECTORY/linux-${VERSION}.tar.xz
else
  wget -O - https://www.kernel.org/pub/linux/kernel/v${KERNEL_SUBDIR}/linux-${VERSION}.tar.xz | tee $DOWNLOADS_DIRECTORY/linux-${VERSION}.tar.xz | tar -xJf -
fi

# Move to the kernel directory:
cd linux-${VERSION}

local FOUND GHOST_SUFFIX i j
# Apply all patches for this version:
for i in $OUR_KERNEL_DIR/linux-{$VERSION,${VERSION%.*}.%,${VERSION%.*.*}.%.%}[.-]*.{diff,patch}; do
  FOUND=y
  j=$(basename $i)
  echo "Applying patch: \'$j'";
  echo "---"
  patch -p1 < $i
  cp $i ./
  if grep -q "ghost" <<<"$j"; then
    GHOST_SUFFIX="-ghost"
  fi
  echo "---"
done
if [[ -z $FOUND ]]; then
  echo "No patch found for this kernel version in $OUR_KERNEL_DIR/kernel"
  echo "At least the \"ghostification\" patch was expected at location $OUR_KERNEL_DIR/linux-${VERSION}-ghost.diff"
  echo "Continuing however without patches."
fi

# Copy or generate .config from our repository
FOUND=$OUR_KERNEL_DIR/CONFIG-$VERSION
if [[ -f $FOUND ]]; then
  echo "Using pre-built config file found at $FOUND"
  cp $FOUND .config
elif [[ -f $OUR_KERNEL_DIR/older-versions/CONFIG-2.6.18 ]]; then
  echo "Config file for version $VERSION not found. We generate it from our older CONFIG-2.6.18"
  create_kernel_config_from $OUR_KERNEL_DIR/older-versions/CONFIG-2.6.18
else
  echo "Error: $OUR_KERNEL_DIR/older-versions/CONFIG-2.6.18 not found"
  return 2
fi

# Modify CONFIG_LOCALVERSION="-ghost" according to the
# presence of the "ghostification" patch:
if [[ -n $GHOST_SUFFIX ]]; then
 sed -i -e 's/CONFIG_LOCALVERSION="-ghost"/CONFIG_LOCALVERSION=""/' .config
 #unset GHOST_SUFFIX
fi

# Custom:
if [[ $CUSTOM = y ]]; then
 local PSEUDO_TERMINAL=$(tty)
 make menuconfig ARCH=um SUBARCH=i386 0<$PSEUDO_TERMINAL 1>$PSEUDO_TERMINAL
fi

# Add `ccache' in the PATH if needed:
if [[ -f /usr/lib/ccache/gcc ]] && ! grep -q "ccache" <<<$PATH; then
  export PATH=$(dirname $(which gcc)):$PATH
fi

# Exploit processors:
local PROCESSOR_NO=$(\grep "^processor.*:" /proc/cpuinfo | sort | uniq | wc -l)

# Launch the compilation process with the virtual `um' architecture (ARCH),
# and with `i386' target host architecture (SUBARCH)
make -j $PROCESSOR_NO ARCH=um SUBARCH=i386

cp -a linux linux-${VERSION}${GHOST_SUFFIX}-unstripped
strip linux
ln linux linux-${VERSION}${GHOST_SUFFIX}
cp .config linux-${VERSION}${GHOST_SUFFIX}.config
echo -ls -l $PWD
ls -l linux-*
popd
} # download_patch_and_compile_kernel


# ---------------
# Testing kernels
# ---------------

# Example:
# start_kernel  ./kernel32-3.2.48  machine-brighella-59975
function start_kernel {
  local GDB
  if [[ $1 = "--debug" ]]; then
    # GDB="gdb --eval-command run --args "
    GDB='gdb -ex "handle SIGSEGV nostop noprint" -ex "handle SIGUSR1 nopass stop print" -ex run --args '
    shift
  fi
  [[ $# -gt 1 ]] || return 1
  local KERNEL="${GDB}${1}"
  local FS=$2
  shift; shift;
  local OTHER_OPTIONS="$@"
  set -x
  TAP=$(LC_ALL=en_US ifconfig -a | \grep -o "^tap[0-9]" | head -n 1)
  if [[ -z $TAP ]]; then
   xterm -l -sb -T "m1" -e "$KERNEL keyboard_layout=us ubda=$FS umid=m1 mem=128M root=98:0 hostname=m1 guestkind=machine $OTHER_OPTIONS"
  else
   xterm -l -sb -T "m1" -e "$KERNEL keyboard_layout=us ubda=$FS umid=m1 mem=128M root=98:0 hostname=m1 guestkind=machine eth0=tuntap,$TAP $OTHER_OPTIONS"
  fi
  # -l generate a log XTerm.log.<DATE>
  set +x
  echo "fuser -k first time:"
  fuser -k ${FS#*,}
  echo "fuser -k second time:"
  fuser -k ${FS#*,}
}

# Example:
# start_kernel_with_cow  ./kernel32-3.2.48  machine-brighella-59975
function start_kernel_with_fresh_cow {
  local GDB
  if [[ $1 = "--debug" ]]; then
    GDB="--debug"
    shift
  fi
  local KERNEL=$1
  local FS=$2
  local COWFILE=$(mktemp /tmp/start_kernel.XXXXXXX.cow)
  rm -f $COWFILE
  FS="$COWFILE,$FS"
  shift; shift;
  start_kernel $GDB $KERNEL $FS "$@"
}


# Stop here if the option -s (--source) has been provided:
if [[ -n ${option_s}  ]]; then
 # Export all functions of this file:
 echo export -f $(awk <$BASH_SOURCE "$AWK_PROGRAM_LISTING_FUNCTIONS")
 export -f $(awk <$BASH_SOURCE "$AWK_PROGRAM_LISTING_FUNCTIONS")
 return 0 2>/dev/null || {
   echo "Warning: the option -s must be used source-ing this script, not when the script is called as a standalone executable";
   echo "Example: source $BASH_SOURCE -s";
   exit 1;
   }
fi

if [[ $# -eq 0 ]]; then
 print_usage_and_exit 2
fi

KERNEL_VERSION="$1"

if ! echo $KERNEL_VERSION | grep -q "^[1-9][.][0-9][0-9]*[.][0-9][0-9]*$"; then
  echo 1>&2 "Error: \`$KERNEL_VERSION' is not a valid kernel version."
  print_usage_and_exit 2
fi

WORKING_DIRECTORY=${2:-.}
DOWNLOADS_DIRECTORY=$WORKING_DIRECTORY/_build.downloads

[[ -d $WORKING_DIRECTORY ]] || {
  echo 1>&2 "Unexisting working directory \`$WORKING_DIRECTORY'"
  echo 1>&2 "Exiting."
  exit 3
}

[[ -d $WORKING_DIRECTORY/linux-$KERNEL_VERSION ]] && {
  echo 1>&2 "A directory \`$WORKING_DIRECTORY/linux-$KERNEL_VERSION' already exists."
  KERNEL_DIR_BACKUP=$WORKING_DIRECTORY/linux-$KERNEL_VERSION.$(date +%Y-%m-%d.%H\h%M | tr -d " ").backup
  mv $WORKING_DIRECTORY/linux-$KERNEL_VERSION $KERNEL_DIR_BACKUP
  echo 1>&2 "Moved to \`$KERNEL_DIR_BACKUP'"
}

set -x
download_patch_and_compile_kernel $CUSTOM_OPTION $KERNEL_VERSION "$WORKING_DIRECTORY" "$DOWNLOADS_DIRECTORY"
set +x

function abspath {
 local B=$(basename $1)
 local D=$(dirname $1)
 (builtin cd $D; echo $PWD/$B)
}

# If we are in the same directory of the script, we switch to a directory name
# beginning with "_build." (according to the Makefile):
if [[ $(dirname $(abspath $WORKING_DIRECTORY)) = $(dirname $(abspath "$0")) ]]; then
  BUILT_DIR=_build.linux-${KERNEL_VERSION}.$(date +%Y-%m-%d.%H\h%M).$RANDOM
  echo "Moving \`$WORKING_DIRECTORY/linux-$KERNEL_VERSION' -> \`$WORKING_DIRECTORY/$BUILT_DIR'"
  mv $WORKING_DIRECTORY/linux-$KERNEL_VERSION $WORKING_DIRECTORY/$BUILT_DIR
  # Copy log:
  cp $LOGFILE $WORKING_DIRECTORY/$BUILT_DIR/$(basename $LOGFILE)
  if [[ -f linux-${VERSION}.tar.xz ]]; then
    mkdir -p $DOWNLOADS_DIRECTORY
    mv linux-${VERSION}.tar.xz $DOWNLOADS_DIRECTORY/
  fi
fi

echo 'Success.'
