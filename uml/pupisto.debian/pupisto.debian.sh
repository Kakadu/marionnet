#!/bin/bash

# This file is part of Marionnet, a virtual network laboratory
# Copyright (C) 2013  Jean vincent Loddo
# Copyright (C) 2013  Antoine Seignard
# Copyright (C) 2013  Université Paris 13

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This script helps people to build a debian filesystem
# with debootstrap according to the Marionnet requirements.

# =============================================================
#                AUTOMATIC LOG-FILE GENERATION
# =============================================================

MY_BASENAME=$(basename $0)
if [[ $1 = "--help" || $1 = "-h" ]]; then
  # do nothing and continue
  :
elif grep -q "log_${MY_BASENAME}[.]......$" <<<"$1"; then
  LOGFILE=$1
  shift
  # and continue
else
  LOGFILE=$(mktemp /tmp/log_${MY_BASENAME}.XXXXXX)
  EXIT_CODE_FILE=$(mktemp /tmp/exit_code_${MY_BASENAME}.XXXXXX)
  echo -e "Log file of command:\n$0" "$@" "\n---" | tee $LOGFILE
  COLUMNS=$(tput cols)
  { time $0 "$LOGFILE" "$@"; echo $? >$EXIT_CODE_FILE; } 2>&1 | tee -a "$LOGFILE" | cut -c1-$((COLUMNS))
  read EXIT_CODE <$EXIT_CODE_FILE
  rm -f $EXIT_CODE_FILE
  echo "---"
  echo "$MY_BASENAME: previous running logged into $LOGFILE"
  exit $EXIT_CODE
fi

# Script body:
set -e

# =============================================================
#                      CMDLINE PARSING
# =============================================================

# Getopt's format used to parse the command line:
OPTSTRING="hmc:Kk:dn:s:r:t:L"

function parse_cmdline {
local i j flag
# Transform long format options into the short one:
for i in "$@"; do
  if [[ double_dash_found = 1 ]]; then
    ARGS+=("$i")
  else case "$i" in
    --help)
      ARGS+=("-h");
      ;;
    --custom)
      ARGS+=("-m");
     ;;
    --continue)
      ARGS+=("-c");
     ;;
    --name)
      ARGS+=("-n");
     ;;
    --kernel)
      ARGS+=("-k");
     ;;
    --no-kernel)
      ARGS+=("-K");
     ;;
    --debug)
      ARGS+=("-d");
     ;;
    --release)
      ARGS+=("-r");
     ;;
    --server)
      ARGS+=("-s");
     ;;
    --fstype)
      ARGS+=("-t");
     ;;
    --no-locales)
      ARGS+=("-L");
     ;;
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
else
  set - "${ARGS[@]}";
fi
unset ARGS

function print_usage_and_exit {
 # global DEFAULT_KERNEL_VERSION  DEFAULT_FSTYPE
 echo "Usage: ${0##*/} [OPTIONS]
Build a debian filesystem suitable for Marionnet.

NOT IMPLEMENTED YET --------
If one or both files \`_build.custom_packages_{yes,no}' exist in the current
directory, their uncommented lines tell the script which Debian's packages
are wanted (yes) or unwanted (no). The list of selected packages results from
the formula:

    (required DIFF no) UNION yes

where \`required' are the few packages considered relevant by Marionnet's
developpers for pedagogical reasons.
----------------------------

Options:
  -r/--release       set the release between \"wheezy\" (default) or \"squeeze\"
  -t/--fstype        set the filesystem type (default \"ext3\")
  -m/--custom        customize the package selection interactively
  -k/--kernel VERS   set the kernel version (for headers and/or to compile)
  -K/--no-kernel     do not compile the kernel
  -c/--continue DIR  continue a previously broken execution in directory DIR
  -s/--server URL    set the HTTP/DIR Debian server
  -L/--no-locales    don't install \"locales\" and purge *.mo files
  -h/--help          Print this message and exit
---
Defaults:
  --release \"wheezy\"
  --kernel ${DEFAULT_KERNEL_VERSION}
  --fstype ${DEFAULT_FSTYPE}
  --server \"http://ftp.debian.org/debian/\"

Notes
  - If the kernel is compiled (you can disable it with -K) it will
    compiled outside the Debian filesystem.
  - We suggest to add a line like
    'Defaults timestamp_timeout=60'
    in your /etc/sudoers, to prevent the script to ask the sudo
    password several times

Examples:
$ ${0##*/} -r wheezy
$ ${0##*/} -k 3.0.8 --custom -t ext4 -r wheezy"
 exit $1
}

# Note that 2.6.30 is the first header's version allowing a
# successfully buildroot compilation (2.6.18 fails)
# Another possible default could be 2.6.32 (last statically-linked
# available version of our patched (ghost2) kernel)
DEFAULT_KERNEL_VERSION=3.2.48

# We set this default to "ext2" in order to obtain smaller filesystems:
DEFAULT_FSTYPE="ext2"

# Manage now your options in a convenient order
#
# Option -h
if [[ -n ${option_h}  ]]; then
 print_usage_and_exit 0
fi
# Option -k --kernel
if [[ -n ${option_k} ]]; then
 KERNEL_VERSION=$option_k_arg
else
 KERNEL_VERSION=$DEFAULT_KERNEL_VERSION
fi
# Option -r --release
if [[ -n ${option_r} ]]; then
 RELEASE=$option_r_arg
else
 RELEASE="wheezy"
fi
# Option -t --fstype
if [[ -n ${option_t} ]]; then
 FSTYPE=$option_t_arg
else
 FSTYPE="$DEFAULT_FSTYPE"
fi
# Option -s --server
if [[ -n ${option_s} ]]; then
 HTTP_SERVER=$option_s_arg
else
 HTTP_SERVER="http://ftp.debian.org/debian/"
fi
# Option -d --debug
if [[ -n ${option_d} ]]; then
 DEBUGGING_MODE=y
fi
# Option -L --no-locales
if [[ -z ${option_L} ]]; then
 INSTALL_LOCALES=y
fi
# Option -c --continue
if [[ -n ${option_c} ]]; then
  if [[ -d "${option_c_arg}" ]]; then
   CONTINUE_IN_DIRECTORY="${option_c_arg}"
   CONTINUE=yes
  else
   echo "Error: ${option_c_arg} doesn't exist or is not a directory."
   echo "Exiting."
   exit 1
  fi
fi

# TODO:
if [[ ! $RELEASE = wheezy ]]; then
  echo "Sorry, currently only \`wheezy' is supported by this script. Exiting."
  exit 1
fi

# =============================================================
#                    DEBUGGING SETUP
# =============================================================

# Source anyway, in order to able to freely leave break-points in the code.
# Defined functions:
#   ___break_point___
#   set_tracing
#   unset_tracing
#   set_debugging
#   unset_debugging
#   set_once_actions_file
#   once
# Referred global:
#  DEBUGGING_MODE
source ../pupisto.common/toolkit_debugging.sh

# In order to enrich the log:
set_tracing

# =============================================================
#                   GENERAL SETUP
# =============================================================

source ../pupisto.common/toolkit_config_files.sh
# Exported functions available from now:
#   tabular_file_update
#   user_config_set
#   set_default_config_file
#   set_default_config_field_separator
#   set_config_variable
#   unset_config_variable
#   get_config_variable
#   get_config_variable_unquoting
# Examples:
# tabular_file_update -d ":" -k 1 --key-value "root" -f 7 --field-value "/bin/bash" --field-old-value "/bin/sh" etc/passwd
# user_config_set "PermitRootLogin" " " "yes" etc/sshd_config

source ../pupisto.common/toolkit_chroot.sh
# Defined functions:
# mkTMPFILE
# list_diff
# rewrite
# save_files
# restore_files
# sudo_fcall
# careful_chroot
# sudo_careful_chroot
# copy_content_into_directory
# sudo_chroot_binary_list

source ../pupisto.common/toolkit_image.sh
# Defined functions:
# rename_with_sum_and_make_image_dot_conf

set -e
set -E # the ERR trap is inherited by shell functions

DISTRIBUTION_NAME=debian-$RELEASE

# Create the temporary working directory $TWDIR or continue in an already created
# directory:
if [[ -n $CONTINUE ]]; then
  TWDIR="$(basename $CONTINUE_IN_DIRECTORY)"
else
  TWDIR=_build.$DISTRIBUTION_NAME-with-linux-${KERNEL_VERSION}.$(date +%Y-%m-%d.%H\h%M)
  mkdir -v -p $TWDIR
  echo "Temporary Working directory: $TWDIR"
fi
DEBIANROOT=$TWDIR/debianroot
mkdir -v -p $DEBIANROOT
PUPISTO_DIR=$PWD

# Initialize the file registering actions to be performed once:
set_once_actions_file $TWDIR/ONCE_ACTIONS_FILE

ARCH=i386
PUPISTO_FILES=pupisto.debian.sh.files

# Unnecessary therefore essential ;-)
INSTALL_LINUXLOGO=y

# =============================================================
#                      DEBOOTSTRAP
# =============================================================

function launch_debootstrap_and_fix_problems_with_apt {
 # global DEBIAN_FRONTEND RELEASE DEBIANROOT HTTP_SERVER INSTALL_LOCALES (input)
 # global INCLUDED_PACKAGES (output)
 local ROOT=${1:-$DEBIANROOT}
 local MANDATORY_PACKAGES="makedev realpath tcpdump openssh-server traceroute"
 local SELECTION=$PUPISTO_FILES/package_catalog/package_catalog.$RELEASE.selection
 # ---
 if [[ $INSTALL_LINUXLOGO = y ]]; then
   MANDATORY_PACKAGES+=" linuxlogo"
 fi
 if [[ $INSTALL_LOCALES = y ]]; then
   MANDATORY_PACKAGES+=" locales"
 fi
 INCLUDED_PACKAGES=$( (for i in $MANDATORY_PACKAGES; do echo $i; done; awk <$SELECTION '$1 !~ /^#/ {print $1}') | sort -d | uniq)
 INCLUDED_PACKAGES=$(echo $INCLUDED_PACKAGES)
 INCLUDED_PACKAGES=${INCLUDED_PACKAGES// /,}

 local EXCLUDED_PACKAGES="udev"
 EXCLUDED_PACKAGES=$(echo $EXCLUDED_PACKAGES)
 EXCLUDED_PACKAGES=${EXCLUDED_PACKAGES// /,}

 # Note: without --foreign
 local INCLUDE="--include=$INCLUDED_PACKAGES"
 local EXCLUDE="--exclude=$EXCLUDED_PACKAGES"

 # Relevant for a non-interactive debootstrap running:
 export DEBIAN_FRONTEND=noninteractive

 # Ask sudo password once, please:
 sudo -v -p "[sudo] password for %u (required for chroot/mount actions): "

 if once --register-anyway sudo debootstrap --no-check-gpg --arch=${ARCH} $INCLUDE $EXCLUDE $RELEASE ${ROOT} ${HTTP_SERVER}; then
   echo "Happily \`debootstrap' managed the installation of all packages!"
 else
   sudo -v # update cached credentials
   echo "I try now to continue with \`apt-get -f install'..."
   once sudo_careful_chroot ${ROOT} apt-get -y --force-yes -f install
 fi
 sudo -v # update cached credentials
}

# =============================================================
#                        TUNING
# =============================================================

function fix_apt_sources_update_and_upgrade {
 # global DEBIANROOT HTTP_SERVER RELEASE
 local ROOT=${1:-$DEBIANROOT}
 local TARGET=$ROOT/etc/apt/sources.list
 local TMPFILE=$(mktemp)
 cat 1>$TMPFILE <<EOF
deb $HTTP_SERVER $RELEASE main
deb http://security.debian.org/ $RELEASE/updates main
EOF
 sudo_fcall rewrite $TARGET with cat $TMPFILE
 rm -f $TMPFILE
 # Update:
 # sudo_careful_chroot ${ROOT} aptitude update
 sudo_careful_chroot ${ROOT} apt-get update
 # Upgrade:
 # sudo_careful_chroot ${ROOT} aptitude -y safe-upgrade
 sudo_careful_chroot ${ROOT} apt-get -y upgrade
}

function install_package {
 # global DEBIANROOT
 # sudo_careful_chroot ${DEBIANROOT} aptitude install -y "$@"
 sudo_careful_chroot ${DEBIANROOT} apt-get install -y "$@"
}

function remove_package {
 # global DEBIANROOT
 # sudo_careful_chroot ${DEBIANROOT} aptitude remove -y "$@"
 sudo_careful_chroot ${DEBIANROOT} apt-get remove -y "$@"
}

# Fix /etc/inittab, i.e.
# (1) turn the line:
# < ca:12345:ctrlaltdel:/sbin/shutdown -t1 -a -r now
# into:
# > ca:12345:ctrlaltdel:/sbin/halt
# (2) add a line:
# > 0:12345:respawn:/sbin/getty 38400 tty0 xterm
# (3) comment lines in the form:
# > #1:2345:respawn:/sbin/getty 38400 tty1
# > #2:23:respawn:/sbin/getty 38400 tty2
# > ...
# > #6:23:respawn:/sbin/getty 38400 tty6
# Note that the function is idempotent.
function fix_etc_inittab {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local TMPFILE=$(mktemp)
 local AWK_PROGRAM='/^1:2345:respawn:/ {print line} /^[1-6]:[1-6]*:respawn/ {print "#"$0; next} {print}'
 # Note that th -L option is very relevant to obtain a console quicly reacting to CTRL-C/CTRL-Z etc:
 sudo awk -v line="0:12345:respawn:/sbin/getty -L 38400 tty0 xterm" "$AWK_PROGRAM" "$ROOT/etc/inittab" > $TMPFILE
 tabular_file_update --ignore-unchanged -i -d ":" -k 1 --key-value "ca" -f 4 --field-value "/sbin/halt" $TMPFILE
 sudo_fcall rewrite $ROOT/etc/inittab with cat $TMPFILE
 rm -f $TMPFILE
}

function fix_etc_fstab {
 # global DEBIANROOT FSTYPE
 local ROOT=${1:-$DEBIANROOT}
 local TYPE=${FSTYPE:-$DEFAULT_FSTYPE}
 local TMPFILE=$(mktemp)
 cat 1>$TMPFILE <<EOF
# Virtual disk partitions:
/dev/ubdb none swap sw 0 0
/dev/ubda / $TYPE defaults 0 0

# Pseudo filesystems:
proc /proc proc nodev,noexec,nosuid 0 0
devpts /dev/pts devpts gid=5,mode=620 0 0
sysfs /sys sysfs defaults 0 0
tmpfs /run/shm tmpfs rw,nosuid,nodev,noexec,relatime,size=1M 0 0
EOF
 sudo_fcall rewrite $ROOT/etc/fstab with cat $TMPFILE
 rm -f $TMPFILE
}

# Set the `root' passwd to "root":
function fix_root_password {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local ENCRYPTED_PASSWORD='$6$guviKk1z$8biiuI/xnimdRLGlb0Zj0A0X6BoMv3rn3RuwOfplx7F6drBEH/DeVh7z2xYBi/9xNaS3dOuJ.49C1PFSmZd7C.'
 export ENCRYPTED_PASSWORD # because sudo_fcall transmits exported variables
 sudo_fcall tabular_file_update -i -d ":" -k 1 --key-value "root" -f 2 --field-value '$ENCRYPTED_PASSWORD' $ROOT/etc/shadow
}

# Set the `student' passwd to "student":
function fix_student_password {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local ENCRYPTED_PASSWORD='$6$OdQbZViU$QUBmwgwlT8yEmyuYeitvgTmqIDpCC4D0o3yzYHfwUdkisnhXCsEVgk3Mz2yX4EtQ2nJs02k5orcAx5thzS/gZ/'
 export ENCRYPTED_PASSWORD # because sudo_fcall transmits exported variables
 sudo_fcall tabular_file_update -i -d ":" -k 1 --key-value "student" -f 2 --field-value '$ENCRYPTED_PASSWORD' $ROOT/etc/shadow
}

function fix_etc_securetty {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local TARGET=$ROOT/etc/securetty
 if ! \grep -q "^tty0$" $TARGET; then
   local TMPFILE=$(mktemp)
   sudo awk '/^tty1$/ {print "tty0"} {print}' "$TARGET" > $TMPFILE
   sudo_fcall rewrite "$TARGET" with cat $TMPFILE
   rm -f $TMPFILE
 fi
}

# Executed by `sudo_chroot_fcall' (i.e. as `root' in a chrooted environment):
# We suppose that the directory containing ethghost in the
# chrooted environment is "/tmp/ethghost":
function make_ethghost {
 local ETHGHOST=/tmp/ethghost
 make -C $ETHGHOST
 strip $ETHGHOST/ethghost
 make DESTDIR=/usr/local -C $ETHGHOST install
 rm -rf $ETHGHOST
}

# Compile ethghost into the 32-bits filesystem.
# Here we suppose that the apt sources have been fixed:
function compile_and_install_ethghost {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 sudo cp -dR ../ethghost $ROOT/tmp/
 sudo chroot ${ROOT} apt-get install -y linux-libc-dev libc6-dev || true
 export -f make_ethghost
 sudo_chroot_fcall ${ROOT} make_ethghost
}

function make_symlink_etc_init_dhcpd_in_chroot {
 if [[ ! -e /etc/init.d/dhcpd  && -x /etc/init.d/isc-dhcp-server ]]; then
  ln -s isc-dhcp-server /etc/init.d/dhcpd;
 fi
}

function make_symlink_etc_init_dhcpd {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 export -f make_symlink_etc_init_dhcpd_in_chroot
 sudo_chroot_fcall ${ROOT} make_symlink_etc_init_dhcpd_in_chroot
}

function install_marionnet_relay_as_root {
 # global PUPISTO_FILES DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 cp -v $PUPISTO_FILES/marionnet-relay ${ROOT}/etc/init.d/
 chmod +x ${ROOT}/etc/init.d/marionnet-relay
 chroot $ROOT update-rc.d marionnet-relay defaults
}

function install_marionnet_relay {
 # global PUPISTO_FILES DEBIANROOT
 export PUPISTO_FILES DEBIANROOT
 sudo_fcall install_marionnet_relay_as_root
}

function install_bashrc_in_the_skeleton {
 # global PUPISTO_FILES DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local BASHRC=$PUPISTO_FILES/bashrc
 sudo cp $BASHRC $ROOT/etc/skel/.bash_aliases
}

function copy_bashrc_in_the_root_home {
 # global PUPISTO_FILES DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local BASHRC=$PUPISTO_FILES/bashrc
 # Make the `root' directory similar to the `student' one about Bash settings:
 sudo cp -f $ROOT/etc/skel/.bash* $ROOT/root/
 # Copy our bashrc (as bash_aliases, that will be sourced by the actual bashrc):
 sudo cp $BASHRC $ROOT/root/.bash_aliases
}

function create_user_student {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 sudo_careful_chroot $ROOT adduser --home /home/student --shell /bin/bash --uid 1001 --disabled-password --gecos "Student,,," student
 fix_student_password $ROOT
 sudo_careful_chroot $ROOT adduser student sudo
}

function configure_daemon_and_accounts_to_accept_ssh_connection_as_root {
 # global DEBIANROOT PUPISTO_FILES
 local ROOT=${1:-$DEBIANROOT}
 SSHD_CONFIG=$ROOT/etc/ssh/sshd_config
 SSH_DIR=$PUPISTO_FILES/ssh
 if [[ -f "$SSHD_CONFIG" ]]; then
  user_config_set --ignore-unchanged "PermitRootLogin"      " " "yes" $SSHD_CONFIG
  user_config_set --ignore-unchanged "StrictModes"          " " "no"  $SSHD_CONFIG
  user_config_set --ignore-unchanged "PubkeyAuthentication" " " "yes" $SSHD_CONFIG
  mkdir -p $ROOT/{home/student,root}/.ssh
  chmod 700 $ROOT/{home/student,root}/.ssh
  chown 1001:1001 $ROOT/home/student/.ssh
  cat ${SSH_DIR}/id_rsa_marionnet.pub >> $ROOT/home/student/.ssh/authorized_keys
  cat ${SSH_DIR}/id_rsa_marionnet.pub >> $ROOT/root/.ssh/authorized_keys
  chmod 644 $ROOT/{home/student,root}/.ssh/authorized_keys
 else
  echo "Warning: $SSHD_CONFIG not found. I can't configure daemon and accounts to accept Marionnet connections."
 fi
} # configure_daemon_and_accounts_to_accept_ssh_connection

function configure_daemon_and_accounts_to_accept_ssh_connection {
 # global PUPISTO_FILES DEBIANROOT
 export PUPISTO_FILES DEBIANROOT
 sudo_fcall configure_daemon_and_accounts_to_accept_ssh_connection_as_root
}

# Fix /etc/issue according to $INSTALL_LINUXLOGO
# Note that the script `marionnet_relay' will catenate the
# output of the command `linuxlogo' with the content of
# the file "/etc/issue.marionnet":
function fix_etc_issue_as_root {
 # global DEBIANROOT INSTALL_LINUXLOGO
 local ROOT=${1:-$DEBIANROOT}
 local PREVIOUS_MESSAGE TARGET
 if [[ $INSTALL_LINUXLOGO = y ]]; then
   unset $PREVIOUS_MESSAGE
   TARGET=$ROOT/etc/issue.marionnet
 else
   # For instance PREVIOUS_MESSAGE could be "Debian GNU/Linux 7"
   PREVIOUS_MESSAGE=$(awk <$ROOT/etc/issue -F "\\" '{print $1; exit 0}')
   PREVIOUS_MESSAGE+="\n"
   TARGET=$ROOT/etc/issue
 fi
 cat >$TARGET <<EOF
${PREVIOUS_MESSAGE}Built with debootstrap and adapted for Marionnet in $(LC_ALL=us date "+%B %Y")
using \`pupisto.debian.sh\' (see the Marionnet source repository)

Use the account root/root or student/student

EOF
}

function fix_etc_issue {
 # global DEBIANROOT
 export DEBIANROOT INSTALL_LINUXLOGO
 sudo_fcall fix_etc_issue_as_root
}


# Note that the option "-a reboot" of the command `exec'
# provokes a deadlock with kernel 3.2.x
# So we replace the line:
# exec -a reboot /sbin/halt "$@" -f
# with the simpler:
# exec /sbin/halt "$@" -f
function fix_reboot_as_root {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}

 # --- Fix `reboot'
 local TARGET=$ROOT/sbin/reboot
 rm -f $TARGET
 cat >$TARGET <<"EOF"
#!/bin/bash

# The option -f prevent a Marionnet crash using kernels 3.2.x
# For a similar reason, the option "-a reboot" must no be used:
exec /sbin/halt "$@" -f
EOF
 # ---
 chmod +x $TARGET

 # --- Fix `shutdown'
 TARGET=$ROOT/sbin/shutdown
 mv $TARGET $TARGET.unsafe
 cat >$TARGET <<"EOF"
#!/bin/bash

# The option -h neutralize the option -r that causes a
# Marionnet crash using kernels 3.2.x
exec -a shutdown /sbin/shutdown.unsafe "$@" -h
EOF
 # ---
 chmod +x $TARGET
} # fix_reboot_as_root

function fix_reboot {
 # global DEBIANROOT
 export DEBIANROOT
 sudo_fcall fix_reboot_as_root
}

function fix_wireshark_init_lua {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 local TARGET=$ROOT/etc/wireshark/init.lua
 if [[ -f $TARGET ]]; then
   sudo sed -i -e 's/dofile(DATA_DIR.."console.lua")/--dofile(DATA_DIR.."console.lua")/' $TARGET
 fi
}

function fix_locales_as_root {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 [[ -n $ROOT ]] || return 1
 # ---
 cat >$ROOT/etc/locale.gen <<"EOF"
# This file lists locales that you wish to have built. You can find a list
# of valid supported locales at /usr/share/i18n/SUPPORTED. Other
# combinations are possible, but may not be well tested. If you change
# this file, you need to rerun locale-gen.
#
# XXX GENERATED XXX
#
# NOTE!!! If you change this file by hand, and want to continue
# maintaining manually, remove the above line. Otherwise, use the command
# "dpkg-reconfigure locales" to manipulate this file. You can manually
# change this file without affecting the use of debconf, however, since it
# does read in your changes.

en_US.UTF-8 UTF-8
EOF
 # ---
 # Equivalent to `dpkg-reconfigure locales':
 chroot $ROOT locale-gen
}

function fix_locales {
 # global DEBIANROOT INSTALL_LOCALES
 [[ -n $INSTALL_LOCALES ]] || return 0
 export DEBIANROOT
 sudo_fcall fix_locales_as_root
}


function prevent_non_vital_services_from_starting {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 # Note that `console-screen.sh' creates a second windows. So, we don't consider it as required.
 # Note that `linuxlogo' is lanched manually by `marionnet_relay'. So, we don't consider it as required.
 # The service `x11-common' should be required for x-nested machines.
 local REQUIRED_SERVICES="bootlogs bootmisc.sh checkroot-bootclean.sh checkroot.sh cron halt hostname.sh killprocs kmod marionnet-relay mountall-bootclean.sh mountall.sh mtab.sh networking procps rc rc.local rcS reboot rmnologin sendsigs single skeleton sudo sysfsutils sysstat umountfs umountroot urandom"
 local TMPFILE1=$(mktemp)
 local TMPFILE2=$(mktemp)
 find $ROOT/etc/init.d/ -mindepth 1 -maxdepth 1 -name "[a-z][a-z.\-0-9]*" -exec basename {} \; > $TMPFILE1
 echo $REQUIRED_SERVICES | tr ' ' '\n' > $TMPFILE2
 SERVICES_TO_REMOVE=$(list_diff $TMPFILE1 $TMPFILE2)
 # Example: SERVICES_TO_REMOVE="apache2 atftpd babeld bind9 checkfs.sh courier-authdaemon courier-imap courier-ldap courier-mta dbus dhcpd etc-setserial hwclock.sh inetutils-inetd inetutils-syslogd isc-dhcp-server lighttpd motd mountdevsubfs.sh mountkernfs.sh mountnfs-bootclean.sh mountnfs.sh nfs-common nfs-kernel-server nis ntp pppd-dns quagga racoon radvd rpcbind rsync setkey setserial slapd snmpd ssh udev udev-mtab umountnfs.sh x11-common"
 rm -f $TMPFILE1 $TMPFILE2
 local i
 for i in $SERVICES_TO_REMOVE; do
   sudo chroot $ROOT update-rc.d -f "$i" remove || true
 done
 # Fix some annoying messages on boot:
 local TARGET=$ROOT/etc/init.d/mountall.sh
 if [[ -f $TARGET ]]; then
  sudo sed -i -e 's/kill -s USR1/kill 2>\/dev\/null -s USR1/' $TARGET
 fi

}

function install_ipv6_care_in_chroot {
 cd /tmp
 wget 'http://sourceforge.net/projects/ipv6-care/files/latest/' -O ipv6_care-latest.tar.gz || return 1
 tar xvzf ipv6_care-latest.tar.gz
 cd ipv6_care-*
 { ./configure && make && make install; } || return 2
}

function install_ipv6_care {
 # global DEBIANROOT
 local ROOT=${1:-$DEBIANROOT}
 export -f install_ipv6_care_in_chroot
 sudo_chroot_fcall $ROOT install_ipv6_care_in_chroot
}

function clean_debian_filesystem {
 # global DEBIANROOT INSTALL_LOCALES TWDIR
 local ROOT=${1:-$DEBIANROOT}
 [[ -n $ROOT ]] || return 1

 # 1) apt-get install deborphan
 echo "* Installing deborphan if necessary...";
 install_package deborphan || true

 # 2) apt-clean
 echo "* Cleaning..."
 sudo chroot "$ROOT" bash -c "apt-get clean; apt-get autoremove"
 sudo chroot "$ROOT" bash -c "dpkg -l | grep '^rc' && COLUMNS=200 dpkg -l | grep '^rc' | awk '{print \$2}' | xargs dpkg -P" || true

 # 3) deborphan
 echo "* Removing orphans..."
 sudo chroot "$ROOT" bash -c "deborphan | xargs apt-get -y --purge remove 2>/dev/null"

 # 4) clean /var/cache
 echo "* Removing all directories in /var/cache..."
 sudo find "$ROOT"/var/cache/* -type f -exec rm -f {} \;

 # 5) clean tmp/
 echo "* Cleaning /tmp and /root..."
 sudo chroot "$ROOT" bash -c ">root/.bash_history; rm -rf tmp/*; rm -rf .rr_moved; >var/mail/root"

 # 6) locales
 if [[ $INSTALL_LOCALES = y ]]; then
   echo "* File *.mo in /usr/share/locale NOT removed"
 else
   echo "* Removing files *.mo from /usr/share/locale..."
   sudo find "$ROOT"/usr/share/locale/ -type f -name "*.mo" -exec rm {} \;
 fi

 # 7) accelerate future 'apt-get update' (very slow with cow-files):
 local TARGET=$ROOT/etc/apt/apt.conf.d/99translations
 sudo_fcall rewrite $TARGET with 'echo Acquire::Languages \"none\"\;'
 sudo find $ROOT/var/lib/apt/lists -name "*i18n_Translation*" -exec rm {} \;

 # 8) remove /var/lib/apt/lists/*
 # sudo chroot "$ROOT" bash -c "apt-get update; aptitude update"
 echo "* Removing /var/lib/apt/lists/* ..."
 sudo rm -rf "$ROOT"/var/lib/apt/lists/*

 # 9) move /debootstrap to $TWDIR
 sudo mv "$ROOT"/debootstrap $TWDIR/
 sudo chown -R ${USER:-nobody} $TWDIR/debootstrap/
 grep "conflict" $TWDIR/debootstrap/debootstrap.log | sort | uniq >> $TWDIR/README.debootstrap.conflicts

 echo "Success."
}

# VERSIONE DI TEST ************************************************************************
# function clean_debian_filesystem {
#  # global DEBIANROOT INSTALL_LOCALES
#  local ROOT=${1:-$DEBIANROOT}
#  [[ -n $ROOT ]] || return 1
#
#  # 2) apt-clean
#  echo "* Cleaning..."
#  sudo chroot "$ROOT" bash -c "apt-get clean; apt-get autoremove"
#
#  # IN PIU:
#  sudo chroot "$ROOT" bash -c "apt-get update; aptitude update"
# #  sudo chroot "$ROOT" bash -c "apt-get upgrade; aptitude safe-upgrade"
#
#  echo "Success."
# }

function make_the_image {
 # global DEBIANROOT IMAGE (or TWDIR and RELEASE)
 local IMAGE=${1:-$TWDIR/machine-debian-${RELEASE}}
 local FS_SIZE IMAGE_SIZE # Megabytes
 local MOUNTDIR
 local RETURN_CODE=0
 # ---
 FS_SIZE=$(sudo du -sm $DEBIANROOT | awk '{print $1}')
 # Add 5%
 let IMAGE_SIZE="FS_SIZE*120/100"
 echo "Creating the image (${IMAGE_SIZE}M) ..."
 dd if=/dev/zero of=$IMAGE bs=1M count=$IMAGE_SIZE
 # ---
 echo "Formatting the image ($FSTYPE) ..."
 sudo mkfs.$FSTYPE -q -F $IMAGE ### TODO: check the existence of mkfs.$FSTYPE at the beginning of the script
 # ---
 MOUNTDIR=$IMAGE.mnt
 mkdir -v -p $MOUNTDIR
 echo "Mounting the image ($MOUNTDIR) ..."
 sudo mount -o loop $IMAGE $MOUNTDIR
 # ---
 echo "Copying the filesystem content into the image..."
 sudo_fcall copy_content_into_directory $DEBIANROOT/ $MOUNTDIR/ || RETURN_CODE=$?
 # ---
 echo "Unmounting the image ($MOUNTDIR) ..."
 sudo umount $MOUNTDIR
 rmdir $MOUNTDIR
 # ---
 if [[ $RETURN_CODE = 0 ]]; then
   echo "Image built into: $IMAGE"
   echo "Success."
 else
   return $RETURN_CODE
 fi
}

# This function is not called directly here but is called by the generic function
# `rename_with_sum_and_make_image_dot_conf' defined in ../pupisto.common/toolkit_image.
function set_X11_SUPPORT_and_related_variables_according_to_choosed_packages {
  # global (input)  DEBIANROOT FSTYPE
  # global (output) X11_SUPPORT MEMORY_MIN_SIZE MEMORY_SUGGESTED_SIZE
  # ---
  # The file compression used with btrfs (lzo) requires more memory:
  if [[ $FSTYPE = btrfs ]]; then
    if sudo chroot $DEBIANROOT dpkg -l | \grep -q "ii[ ][ ]*libx11-"; then
      if sudo chroot $DEBIANROOT dpkg -l | \grep -q "ii[ ][ ]*xserver-"; then
	X11_SUPPORT="xnested"
	MEMORY_MIN_SIZE=128
	MEMORY_SUGGESTED_SIZE=192
      else
	X11_SUPPORT="xhosted"
	MEMORY_MIN_SIZE=96 # tested
	MEMORY_SUGGESTED_SIZE=128
      fi
    else
      X11_SUPPORT="none"
      MEMORY_MIN_SIZE=32 # tested
      MEMORY_SUGGESTED_SIZE=40
    fi
  else # Normal case (no btrfs):
    if sudo chroot $DEBIANROOT dpkg -l | \grep -q "ii[ ][ ]*libx11-"; then
      if sudo chroot $DEBIANROOT dpkg -l | \grep -q "ii[ ][ ]*xserver-"; then
	X11_SUPPORT="xnested"
	MEMORY_MIN_SIZE=64
	MEMORY_SUGGESTED_SIZE=128
      else
	X11_SUPPORT="xhosted"
	MEMORY_MIN_SIZE=64 # tested
	MEMORY_SUGGESTED_SIZE=80 # tested
      fi
    else
      X11_SUPPORT="none"
      MEMORY_MIN_SIZE=24 # tested
      MEMORY_SUGGESTED_SIZE=32
    fi
  fi # btrfs or not
}

function mknod_for_virtual_disks_in_chroot {
 local i j p
 rm -f /dev/ubd*
 # ---
 for i in {0..7}; do mknod /dev/ubd$i b 98 $((i*16)); done
 # Generated:
 #  mknod /dev/ubd0 b 98 0
 #  mknod /dev/ubd1 b 98 16
 #  ...
 #  mknod /dev/ubd7 b 98 112

 let j=0; for i in {a..h}; do mknod /dev/ubd$i b 98 $((j*16)); let j=j+1; done
 # Generated:
 #  mknod /dev/ubda b 98 0
 #  mknod /dev/ubdb b 98 16
 #  ...
 #  mknod /dev/ubdh b 98 112

 let j=0; for i in {a..h}; do for p in {1..4}; do mknod /dev/ubd$i$p b 98 $((j*16+p)); done; let j=j+1; done
 # Generated:
 # ---
 #  mknod /dev/ubda1 b 98 1
 #  mknod /dev/ubda2 b 98 2
 #  mknod /dev/ubda3 b 98 3
 #  mknod /dev/ubda4 b 98 4
 #
 #  mknod /dev/ubdb1 b 98 17
 #  mknod /dev/ubdb2 b 98 18
 #  mknod /dev/ubdb3 b 98 19
 #  mknod /dev/ubdb4 b 98 20
 #  ...
 #  mknod /dev/ubdh1 b 98 113
 #  mknod /dev/ubdh2 b 98 114
 #  mknod /dev/ubdh3 b 98 115
 #  mknod /dev/ubdh4 b 98 116
}

function mknod_for_virtual_disks {
 # global DEBIANROOT PUPISTO_FILES
 local ROOT=${1:-$DEBIANROOT}
 export -f mknod_for_virtual_disks_in_chroot
 sudo_chroot_fcall $ROOT mknod_for_virtual_disks_in_chroot
 sudo tar -C $ROOT -xz --keep-old-files -f $PUPISTO_FILES/dev.tar.gz
}

# =============================================================
#                         KERNEL
# =============================================================

function make_or_link_the_kernel {
 # global option_K TWDIR KERNEL_VERSION
 local EXISTING_KERNEL_DIR BUILT_DIR
 local DOWNLOADS_DIRECTORY=../pupisto.kernel/_build.downloads
 # ---
 # Option -K/--no-kernel
 if [[ -z ${option_K} ]]; then
  EXISTING_KERNEL_DIR=$(find ../pupisto.kernel/ -maxdepth 1 -type d -name "_build.linux-$KERNEL_VERSION*" | sort | tail -n 1)
  if [[ -d $EXISTING_KERNEL_DIR ]]; then
    echo 1>&2 "A directory \`$EXISTING_KERNEL_DIR' already exists: making a symlink to!"
    ln -s ../"$EXISTING_KERNEL_DIR" "$TWDIR/linux-$KERNEL_VERSION"
  else
    # In order to have a unique log, we will use the script as
    # a library of functions instead of as a standalone program:
    source ../pupisto.kernel/pupisto.kernel.sh --source
    # Now call the function:
    download_patch_and_compile_kernel ${KERNEL_VERSION} ${TWDIR} ${DOWNLOADS_DIRECTORY}
    # Move the whole directory to the good place (../pupisto.kernel/)
    # in order to potentially share it among other filesystem building:
    BUILT_DIR=_build.linux-${KERNEL_VERSION}.$(date +%Y-%m-%d.%H\h%M).$RANDOM
    echo "Moving \`$TWDIR/linux-$KERNEL_VERSION' -> \`../pupisto.kernel/$BUILT_DIR'"
    mv $TWDIR/linux-$KERNEL_VERSION ../pupisto.kernel/$BUILT_DIR
    ln -s ../../pupisto.kernel/$BUILT_DIR $TWDIR/linux-$KERNEL_VERSION
  fi
 fi
} # make_or_link_the_kernel


# =============================================================
#                        ACTIONS
# =============================================================

# The first step is to create the Debian directory with debootstrap:
once launch_debootstrap_and_fix_problems_with_apt

# Fix apt sources, update and upgrade:
once fix_apt_sources_update_and_upgrade

# Fix locales (if installed) to "en_US.UTF-8":
once fix_locales

# Remove package `udev' (and packages depending to)
once remove_package udev || true

# Fix /etc/inittab:
once fix_etc_inittab

# Fix /etc/securetty:
once fix_etc_securetty

# Fix /etc/fstab:
once fix_etc_fstab

# Fix the problem of /sbin/reboot and /sbin/shutdown that may
# provoke a Marionnet crash:
once fix_reboot

# Set the `root' passwd to "root":
once fix_root_password

# Compile ethghost into the 32-bits filesystem:
once compile_and_install_ethghost # <=

# Make a symlink: /etc/init.d/dhcpd -> isc-dhcp-server
once make_symlink_etc_init_dhcpd

# Install our marionnet-startup script:
once install_marionnet_relay # <=

# Install our bashrc into $DEBIANROOT/etc/skel/
once install_bashrc_in_the_skeleton

# Copy our bashrc in the root's home directory:
once copy_bashrc_in_the_root_home

# Create user "student" (with the modified skeleton):
once create_user_student

# Configure the ssh daemon and root/student accounts to accept Marionnet connections:
once configure_daemon_and_accounts_to_accept_ssh_connection

# Login message.
once fix_etc_issue

# Do not start services at boot (except the vital ones):
once prevent_non_vital_services_from_starting

# Create devices /dev/ubd? for virtual disks:
once mknod_for_virtual_disks

# Prevent a noising warning window to appear when
# wireshark is called as root (that is usual with
# Marionnet:
once fix_wireshark_init_lua

# Install this nice program, useful for labs about IPv6 compliance:
once install_ipv6_care

# Final cleaning:
once clean_debian_filesystem

# Get the binary list:
BINARY_LIST=$(sudo_chroot_binary_list $DEBIANROOT)

# Go:
FS_LOC=$TWDIR/machine-debian-${RELEASE}
once make_the_image "$FS_LOC"

# Build image's meta-data.
# This call defines FS_NAME
once rename_with_sum_and_make_image_dot_conf "$FS_LOC"

# Make now the kernel or just link it:
once make_or_link_the_kernel $KERNEL_VERSION

# =============================================================
#                         GREETINGS
# =============================================================

# Store the log file into the output directory:
# make_a_human_readable_log_into_working_directory

# [[ -f $CUSTOM_PACKAGES_NO  ]] && mv $CUSTOM_PACKAGES_NO  $TWDIR/
# [[ -f $CUSTOM_PACKAGES_YES ]] && mv $CUSTOM_PACKAGES_YES $TWDIR/

echo "---"
ls -l $TWDIR
echo "---"
echo "Pay attention to move (or copy with option \`-a') the filesystem in order to preserve the MTIME."
echo "If something goes wrong installing your filesystem, you can restore the correct"
echo "MTIME with the following command:"
echo "sudo touch -d \$(date -d '@$MTIME') $FS_NAME"
echo
echo "Success."

# Copy log and exit:
cat $LOGFILE > $TWDIR/$(basename $LOGFILE)
exit 0


