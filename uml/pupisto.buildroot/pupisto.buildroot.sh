#!/bin/bash

# This file is part of marionnet
# Copyright (C) 2013  Jean-Vincent Loddo
# Copyright (C) 2013  Antoine Seignard
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

# File system creation based on Builroot (itself based on Busybox) and
# kernel patch (ghostification) and compilation

# This script builds from scratch a filesystem with the buildroot utilities.
# Be careful because sometimes buildroot needs some extras packages according
# to the choosen configuration.

# Authors:
#   Jean-Vincent Loddo
#   Antoine Seignard (prototypal version, minor changes, knowhow to add extra
#     buildroot packages)

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
OPTSTRING="hdf:mqrKk:n:c:"

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
    --config)
      ARGS+=("-f");
     ;;
    --continue)
      ARGS+=("-c");
     ;;
    --quagga)
      ARGS+=("-q");
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
    --router)
      ARGS+=("-r");
     ;;
    --debug)
      ARGS+=("-d");
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
 # global DEFAULT_KERNEL_VERSION
 echo "Usage: ${0##*/} [OPTIONS] [<BUILDROOT-ORIGINAL-DIR>]
Build a basic filesystem for Marionnet based on Buildroot, itself based on
Busybox.

If one or both files \`_build.custom_packages_{yes,no}' exist in the current
directory, their uncommented lines tell the script which Buildroot's packages
are wanted (yes) or unwanted (no). The list of selected packages results from
the formula:

  ((config UNION required) DIFF no) UNION yes UNION bash

where \`config' is the default Buildroot configuration file, or the file
provided with the option \`--config', and \`required' are packages considered
relevant by Marionnet's developpers for pedagogical reasons.
Note that, except \`bash', the priority is given to uncommented names
in \`_build.custom_packages_{yes,no}'.

Options:
  -n/--name NAME    set the name of the filesystem (used for /etc/issue)
  -f/--config FILE  use this Buildroot's (starting) configuration file
  -m/--custom       customize the package selection with 'make menuconfig'
  -q/--quagga       include quagga to the filesystem
  -k/--kernel VERS  set the kernel version (for headers and/or to compile)
  -K/--no-kernel    do not compile the kernel
  -r/--router-conf  create also \"router_*.conf\" file (implies -q)
  -c/--continue DIR continue a previously broken execution in directory DIR
  -h/--help         Print this message and exit
---
Defaults:
  <BUILDROOT-ORIGINAL-DIR> is \"./_build.buildroot\".
  --kernel ${DEFAULT_KERNEL_VERSION} is set by default

Notes
  - If the kernel is compiled (you can disable it with -K) it will
    compiled outside Buildroot

  - The router configuration (-r) may be useful to have a unique filesystem
    for both routers and (basic) machines. You can simply install the router
    filesystem creating a (physical or symbolic) link to the related file
    'machine-*'.

Examples:
$ ${0##*/} --no-kernel --kernel 3.0.8 --name pulcinella
$ ${0##*/} --custom --no-kernel --quagga --name pulcinella
$ ${0##*/} --no-kernel --router --name pulcinella"
 exit $1
}

# Note that 2.6.30 is the first header's version allowing a
# successfully buildroot compilation (2.6.18 fails)
# Another possible default could be 2.6.32 (last statically-linked
# available version of our patched (ghost2) kernel)
DEFAULT_KERNEL_VERSION=3.2.51

# Manage now your options in a convenient order
#
# Option -h
if [[ -n ${option_h}  ]]; then
 print_usage_and_exit 0
fi
# Option -n --name
if [[ -n ${option_n} ]]; then
 DISTRIBUTION_NAME=$option_n_arg
fi
# Option -f --config
if [[ -n ${option_f} ]]; then
 CONFIG=$option_f_arg
fi
# Option -k --kernel
if [[ -n ${option_k} ]]; then
 KERNEL_VERSION=$option_k_arg
else
 KERNEL_VERSION=$DEFAULT_KERNEL_VERSION
fi
# Option -r --router. It implies --quagga (-q):
if [[ -n ${option_r} ]]; then
 option_q=y
fi
# Option -d --debug
if [[ -n ${option_d} ]]; then
 DEBUGGING_MODE=y
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

BUILDROOT_ORIG=${1:-./_build.buildroot}
if [[ ! -d $BUILDROOT_ORIG ]]; then
  echo "Perform before 'make ./buildroot' please!"
  exit 2
fi

# Annexes of this script:
PUPISTO_FILES=pupisto.buildroot.sh.files

# Distribution and filesystem names:
if [[ -z $DISTRIBUTION_NAME ]]; then
 DISTRIBUTION_NAME="buildroot-for-kernel-${KERNEL_VERSION}"
fi
FS_NAME="machine-$DISTRIBUTION_NAME"

# Create the temporary working directory $TWDIR or continue in an already created
# directory:
if [[ -n $CONTINUE ]]; then
  TWDIR="$(basename $CONTINUE_IN_DIRECTORY)"
  BUILDROOT=$TWDIR/buildroot
  [[ -d $BUILDROOT ]] || cp -a $BUILDROOT_ORIG $BUILDROOT
else
  TWDIR=_build.$DISTRIBUTION_NAME-with-linux-${KERNEL_VERSION}.$(date +%Y-%m-%d.%H\h%M)
  mkdir -v -p $TWDIR
  BUILDROOT=$TWDIR/buildroot
  cp -a $BUILDROOT_ORIG $BUILDROOT
fi
PUPISTO_DIR=$PWD

# Initialize the file registering actions to be performed once:
set_once_actions_file $TWDIR/ONCE_ACTIONS_FILE


# Will be called at the end of the script or exiting because of an error:
function make_a_human_readable_log_into_working_directory {
 # global TWDIR LOGFILE BUILDROOT PUPISTO_DIR

 if [[ -f $LOGFILE ]]; then
   local READABLE_LOG_FILE
   local PATH_TO_BE_SIMPLIFIED
   local TMPFILE1=$(mktemp)
   local TMPFILE2=$(mktemp)
   # Example:
   # "/home/user/repos/marionnet/uml/pupisto.buildroot/"             replaced by '$PUPISTO_DIR'
   # "_build.brighella-with-linux-3.2.44.2013-05-10.23h06/buildroot" replaced by '$BUILDROOT'
   READABLE_LOG_FILE=$TWDIR/$(basename $LOGFILE)

   PATH_TO_BE_SIMPLIFIED=$(sed 's/\//\\\//g' <<<"$PWD/$BUILDROOT")
   sed <$LOGFILE -e 's/'"$PATH_TO_BE_SIMPLIFIED"'/$BUILDROOT/g' > $TMPFILE1

   PATH_TO_BE_SIMPLIFIED=$(sed 's/\//\\\//g' <<<"$BUILDROOT")
   sed <$TMPFILE1 -e 's/'"$PATH_TO_BE_SIMPLIFIED"'/$BUILDROOT/g' > $TMPFILE2

   PATH_TO_BE_SIMPLIFIED=$(sed 's/\//\\\//g' <<<"$PUPISTO_DIR")
   sed <$TMPFILE2 -e 's/'"$PATH_TO_BE_SIMPLIFIED"'/$PUPISTO_DIR/g' > $READABLE_LOG_FILE
   echo "---"
   echo "Log file made more human-readable and copied into $READABLE_LOG_FILE"
   rm -f $TMPFILE1 $TMPFILE2
 else
   printf "Log file %s not found\n" $LOGFILE 1>&2
 fi
}

function exiting_because_error {
 # global TWDIR LOGFILE
 echo -e "Exiting because of an unexpected error in line $BASH_LINENO"
 make_a_human_readable_log_into_working_directory
 exit 3
}
#
trap exiting_because_error ERR

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


# =============================================================
#                      Definition of
#                 OUR BUILDROOT TUNING SCRIPT
# =============================================================

MARIONNET_RELAY=$PWD/$PUPISTO_FILES/S90marionnet-relay
QUAGGA_DIR=$PWD/$PUPISTO_FILES/quagga
OUR_TUNING_SCRIPT=$PWD/$(mktemp --tmpdir=$TWDIR ROOTFS_POST_IMAGE_SCRIPT.XXXXXXXX)
IFCONFIG_WRAPPER=$PWD/$PUPISTO_FILES/ifconfig
ZTOOLS_WRAPPER=$PWD/$PUPISTO_FILES/zgrep_zdiff_zless
GROUPS_WRAPPER=$PWD/$PUPISTO_FILES/groups
BASHRC=$PWD/$PUPISTO_FILES/bashrc
SSH_DIR=$PWD/$PUPISTO_FILES/ssh
RADVD_DIR=$PWD/$PUPISTO_FILES/radvd
DHCPD_DIR=$PWD/$PUPISTO_FILES/dhcpd
REBOOT_WRAPPER=$PWD/$PUPISTO_FILES/reboot
LINUXLOGO=$PWD/$PUPISTO_FILES/gnu_linux_logo

cat 1>$OUR_TUNING_SCRIPT <<EOF
#!/bin/bash

set -x

# Buildroot launches this script with:
# \$1=\$BUILDROOT/output/images
# \$TARGET_DIR=\$BUILDROOT/output/target

cd \$TARGET_DIR

# Copying our init script
cp ${MARIONNET_RELAY} etc/init.d/

# ifconfig wrapper (only need if we dont install the net-tools package)
if [[ -L sbin/ifconfig && -f $IFCONFIG_WRAPPER ]]; then
  mv sbin/ifconfig sbin/ifconfig.busybox
  cp $IFCONFIG_WRAPPER sbin/ifconfig
  chmod +x sbin/ifconfig
fi

# sh -> bash
if [[ -L bin/sh && -x bin/bash ]]; then
  ln -sf bash bin/sh
fi

# Note that the user "student" is previously created by Buildroot:
cp $BASHRC root/.bashrc
cp $BASHRC home/student/.bashrc
cp root/.bash_profile home/student/

# Also change root shell (in order to read ~/.bashrc):
tabular_file_update -i -d ":" -k 1 --key-value "root" -f 7 --field-value "/bin/bash" etc/passwd || [[ $? -eq 1 ]]

# Poor-man "z"-tools:
ZTOOLS="zgrep zegrep zfgrep zless zdiff zcmp zhead ztail"
for i in \$ZTOOLS; do
  if [[ ! -e bin/\$i ]]; then
    cp $ZTOOLS_WRAPPER bin/\$i
    chmod +x bin/\$i
    FOUND=\$i;
    break;
  fi
done
for i in \$ZTOOLS; do
  if [[ ! -e bin/\$i ]] && [[ ! \$i = \$FOUND ]]; then
  ln -s \$FOUND bin/\$i
  fi
done

# groups utility
if [[ ! -e bin/groups ]] && [[ ! -e usr/bin/groups ]] && [[ -f $GROUPS_WRAPPER ]]; then
  cp $GROUPS_WRAPPER bin/groups
  chmod +x bin/groups
fi

# Adapt the ssh daemon to allow Marionnet to login or execute remote commands:
SSHD_CONFIG=\$(find -type f -name "sshd_config") # should be etc/sshd_config
if [[ -f "\$SSHD_CONFIG" ]]; then
  user_config_set "PermitRootLogin"      " " "yes" \$SSHD_CONFIG
  user_config_set "StrictModes"          " " "no"  \$SSHD_CONFIG
  user_config_set "PubkeyAuthentication" " " "yes" \$SSHD_CONFIG
  mkdir -p {home/student,root}/.ssh
  chmod 700 {home/student,root}/.ssh
  cat $SSH_DIR/id_rsa_marionnet.pub >> home/student/.ssh/authorized_keys
  cat $SSH_DIR/id_rsa_marionnet.pub >> root/.ssh/authorized_keys
  chmod 644 {home/student,root}/.ssh/authorized_keys
fi

# radvd
RADVD=\$(find -type f -name "radvd")
if [[ -f "\$RADVD" ]]; then
  cp $RADVD_DIR/radvd.conf.example* etc/
fi

# reboot
REBOOT=\$(find sbin/ -name "reboot")
if [[ -f "\$REBOOT" ]]; then
  rm $REBOOT
  cp $REBOOT_WRAPPER sbin/reboot
  chmod +x sbin/reboot
fi

# dhcpd
DHCPD=\$(find etc/init.d/ -maxdepth 1 -type f -name "*dhcp-server")
if [[ -f \$DHCPD ]]; then
  # Take our version:
  cat $DHCPD_DIR/dhcp-server >\$DHCPD
fi

# lighttpd
if [[ -d var/www/ ]]; then
 echo '<html>
<body>
<h1>It works!</h1>
</body>
</html>' >var/www/index.html
fi

# Login message.
# Note that setting BR2_TARGET_GENERIC_ISSUE no gives the expected effect,
# so we write the message directly in the good place:
# # # cat >etc/issue <<\EOF_issue
# # # #######################################################
# # # Welcome to \`$DISTRIBUTION_NAME', a compact GNU/Linux filesystem
# # # conceived for Marionnet, based on Busybox and made with
# # # Buildroot ($(LC_ALL=us date "+%B %Y")).
# # # #######################################################
# # # Running with kernel \r
# # #
# # # Use the account root/root or student/student
# # #
# # # EOF_issue
#
cp $LINUXLOGO etc/issue
sed -i -e "s/DISTRIBUTION_NAME/$DISTRIBUTION_NAME/" etc/issue
sed -i -e "s/DATE/$(LC_ALL=us date "+%B %Y")/" etc/issue

EOF

# Append some treatments for quagga:
if [[ -n ${option_q} ]]; then
cat 1>>$OUR_TUNING_SCRIPT <<EOF
# Quagga:
cp $QUAGGA_DIR/quagga etc/init.d/
# Called by ${MARIONNET_RELAY} if the filesystem name starts with 'router-'
chmod +x etc/init.d/quagga
mkdir -p etc/quagga
cp -f $QUAGGA_DIR/{*.conf,README.ports} etc/quagga/
# Add group and system user:
echo "quaggavty:x:116:" >> etc/group
echo "quagga:x:117:" >> etc/group
# TODO: FIX /home/quagga
echo "quagga:x:108:117:Linux User,,,:/home/quagga:/bin/false" >> etc/passwd
echo 'quagga:!:15815:0:99999:7:::' >> etc/shadow
# Fix quagga ownership:
# # # chown -R 108:117 etc/quagga # NOT PERMITTED!!! => to do in marionnet_relay!
EOF
fi

# We dont know how to exploit this information:
# FAKEROOT_SCRIPT=$BUILDROOT/output/build/_fakeroot.fs

# Append other things to do as is (without interpretation, see "EOF"):
cat 1>>$OUR_TUNING_SCRIPT <<"EOF"
# Creating 'student/student':
# Ask Buildroot to do it:
# Source: http://lists.busybox.net/pipermail/buildroot/2012-December/064450.html
# +============+=====+=======+=====+==========+=============+=========+=======+===========+
# |  username  | uid | group | gid | password |   home      | shell   |groups |  comment  |
# +============+=====+=======+=====+==========+=============+=========+=======+===========+
# User `student' defined into `pupisto.buildroot.sh.files/ethghost/ethghost.mk'

# Add student to the `sudo' group. TODO: ensure that the line exists!
TMPFILE=$(mktemp)
awk <etc/sudoers >$TMPFILE '$1=="#" && $2=="%sudo" && $3=="ALL=(ALL)" {print $2,$3,$4; next} {print}'
chmod u+w  etc/sudoers
cat $TMPFILE >etc/sudoers
chmod u-w  etc/sudoers
echo 'sudo:x:27:student' >> etc/group

# Remove the symlink /etc/resolv.conf
rm etc/resolv.conf
>etc/resolv.conf

# Change the `ctrlaltdel' behaviour: no `reboot' but `halt'. This setting is
# very relevant because with `reboot' Marionnet will not be able to cleanly
# shutdown the machine:
sed -i -e 's/::ctrlaltdel:\/sbin\/reboot/::ctrlaltdel:\/sbin\/halt/' etc/inittab

# Add tty0 as available root console:
echo 'tty0' >> etc/securetty

# Add a wrapper `dhclient -> udhcpc (busybox)' if needed:
if [[ -e sbin/udhcpc && ! -e sbin/dhclient ]]; then
 echo '#!/bin/bash
eval exec -a udhcpc busybox -fnq "$@"
' > sbin/dhclient
 chmod +x sbin/dhclient
fi

# Do not start services at boot (except S90marionnet-relay and some other):
pushd etc/init.d/
for i in $(find . -maxdepth 1 -name "S*" -a ! -name "S90marionnet-relay" -a ! -name "S01logging" -a ! -name "S20urandom" -a ! -name "S40network"); do
  j=${i#./S??};
  mv $i $j;
  echo "$j was $i" >> README.script_order
done
# Create links for the remaining services:
for i in $(find . -maxdepth 1 -name "S*" -a ! -name "S90marionnet-relay"); do
  j=${i#./S??};
  mv $i $j;
  ln -s $j $i
done
popd

EOF
# Make it executable and bind it with the Buildroot process:
chmod +x $OUR_TUNING_SCRIPT
# Do not execute our script the first time:
# set_config_variable "BR2_ROOTFS_POST_BUILD_SCRIPT" '"'$OUR_TUNING_SCRIPT'"'


# =============================================================
#                   BUILDROOT PATCHES
# =============================================================

# Package `net-tools':
if [[ -d $BUILDROOT/package/net-tools ]]; then
  echo "No need to apply the \`net-tools' buildroot patch. Fine."
else
  echo "Applying the \`net-tools' buildroot patch."
  patch -d $BUILDROOT -p1 <$PUPISTO_FILES/net-tools.patch
fi


# =============================================================
#                BUILDROOT CONFIGURATION (Step 1)
#                 working on `project_defconfig'
# =============================================================

# Antoine: when you update Buildroot the new configuration options appear,
# so if you copy an old .config and start the build, Buildroot
# will ask you the values for the new options. In order to workaround
# this behaviour, select automatically the default value for the new options, then, do:
# cp your.config.file /path/to/buildroot/sources/configs/project_defconfig &&
#   make project_defconfig && make

if [[ -z ${CONFIG} ]]; then
 >$BUILDROOT/configs/project_defconfig
else
 cp -fv $CONFIG $BUILDROOT/configs/project_defconfig
fi

# Set the initial config file we will working on
# until the next `make project_defconfig':
set_default_config_file $BUILDROOT/configs/project_defconfig

# BR2_ARCH corresponds to SUBARCH during kernel compilation
set_config_variable "BR2_ARCH" '"i386"'
set_config_variable "BR2_i386" "y"
set_config_variable "BR2_ENDIAN" '"LITTLE"'
set_config_variable "BR2_GCC_TARGET_TUNE" '"i386"'
set_config_variable "BR2_GCC_TARGET_ARCH" '"i386"'
set_config_variable "BR2_x86_i386" "y"
set_config_variable "BR2_TARGET_GENERIC_ROOT_PASSWD" '"root"'

# /etc/inittab is built according to the following three lines
# according to the compiled kernel:
set_config_variable "BR2_TARGET_GENERIC_GETTY_PORT" '"tty0"'
set_config_variable "BR2_TARGET_GENERIC_GETTY_BAUDRATE_38400" "y"
unset_config_variable "BR2_TARGET_GENERIC_GETTY_TERM"

##########################################
#  (Step 1) Improve building efficiency  #
##########################################

# Share and reuse DOWNLOAD results among separate Buildroot builds:
BUILDROOT_DL_DIR="$HOME/.buildroot-downld"
set_config_variable "BR2_DL_DIR" '"'$BUILDROOT_DL_DIR'"'
mkdir -p $BUILDROOT_DL_DIR
ln -sf $BUILDROOT_DL_DIR "_build.buildroot-downld"

# Share and reuse COMPILATION results among separate Buildroot builds
# (`ccache' support); fix PATH (if needed) and set BR2_CCACHE:
if [[ -e /usr/lib/ccache/gcc ]]; then
  if ! grep -q "ccache" <<<$PATH; then
    export PATH=$(dirname $(which gcc)):$PATH
  fi
  set_config_variable "BR2_CCACHE" "y"
  mkdir -p "$HOME/.buildroot-ccache"
  ln -sf "$HOME/.buildroot-ccache" "_build.buildroot-ccache"
fi

############################################
#  (Step 1) Additional Buildroot packages  #
############################################

# This sub-section is about adding custom packages, library, or applications
# in buildroot.

# Example:
# add_extra_buildroot_package $PUPISTO_FILES/ethghost $ETHGHOST_VERSION ../ethghost
# add_extra_buildroot_package $PUPISTO_FILES/bind
function add_extra_buildroot_package {
  # global BUILDROOT BUILDROOT_DL_DIR
  local PACKAGE_DEFINITION_DIR=$1
  local PACKAGE_VERSION=$2 # optional
  local SOURCE_DIR=$3 # optional
  #---
  local NAME=$(basename $PACKAGE_DEFINITION_DIR)
  local CONFIG_IN=$PACKAGE_DEFINITION_DIR/Config.in
  local PACKAGE_MK=$PACKAGE_DEFINITION_DIR/$NAME.mk

  [[ -f $CONFIG_IN  ]]      || { echo "Expected file $CONFIG_IN doesn't exist" 1>&2;    return 1; }
  [[ -f $PACKAGE_MK ]]      || { echo "Expected file $PACKAGE_MK doesn't exist" 1>&2;   return 1; }

  local UPPER_NAME=$(echo $NAME | tr '[a-z]' '[A-Z]')
  mkdir -p $BUILDROOT/package/$NAME

  # Simulate the package download if the SOURCE_DIR is given:
  if [[ -n $SOURCE_DIR ]]; then
    tar -C $(dirname $SOURCE_DIR) -czf $BUILDROOT_DL_DIR/${NAME}-${PACKAGE_VERSION}.tar.gz $NAME/
  fi

  # Copy all things as they are from $PACKAGE_DEFINITION_DIR:
  cp $PACKAGE_DEFINITION_DIR/* $BUILDROOT/package/$NAME/

  # Creation of the config.in needed for package description and dependencies.
  cp -f $CONFIG_IN  $BUILDROOT/package/$NAME/

  # Copy $PACKAGE_MK as is or trying to update the version number:
  if [[ -z $PACKAGE_VERSION ]]; then
    cp -f $PACKAGE_MK $BUILDROOT/package/$NAME/$NAME.mk
  else
    awk <$PACKAGE_MK >$BUILDROOT/package/$NAME/$NAME.mk \
      -v version="$PACKAGE_VERSION" '$1 ~ /^[A-Z0-9]*_VERSION$/ {print $1,$2,version; next} {print}'
  fi

  #Append the package/Config.in file to make appear our new package
  cat 1>>$BUILDROOT/package/Config.in<<EOF
     menu "Custom packages"
     source "package/${NAME}/Config.in"
     endmenu
EOF
  local BR2_VARNAME
  # Select the package:
  BR2_VARNAME=$(awk <$CONFIG_IN '/^[\t ]*select/ && ($2~/BR2_PACKAGE_/) {print $2}')
  if [[ -z $BR2_VARNAME ]]; then
    echo "Warning: the file $CONFIG_IN should contain a line 'select BR2_PACKAGE_${UPPER_NAME}'"
    set_config_variable "BR2_PACKAGE_${UPPER_NAME}" "y"
  else
    set_config_variable "$BR2_VARNAME" "y"
    [[ "$BR2_VARNAME" = "BR2_PACKAGE_${UPPER_NAME}" ]] || {
      echo "Warning: the file $CONFIG_IN contain a line 'select $BR2_VARNAME' and it's really fine,\nbut a line 'select BR2_PACKAGE_${UPPER_NAME}' was expected. Is this settings intentional?" 1>&2
      }
  fi
}


# Add now our package `ethghost'
ETHGHOST_VERSION=$(\make --quiet -C ../ethghost print_version)
add_extra_buildroot_package $PUPISTO_FILES/ethghost $ETHGHOST_VERSION ../ethghost

#######################################
#    (Step 1) Packages' selection     #
#######################################

# Add some essential packages:
set_config_variable "BR2_PACKAGE_BUSYBOX" "y"
set_config_variable "BR2_PACKAGE_BUSYBOX_SHOW_OTHERS" "y"
set_config_variable "BR2_PACKAGE_BASH" "y"
# set_config_variable "BR2_PACKAGE_BRIDGE_UTILS" "y" # busybox!
# set_config_variable "BR2_PACKAGE_BZIP2" "y" # busybox!
set_config_variable "BR2_PACKAGE_HOST_E2FSPROGS" "y"
set_config_variable "BR2_PACKAGE_DHCP" "y"
set_config_variable "BR2_PACKAGE_DHCP_SERVER" "y"
# set_config_variable "BR2_PACKAGE_DHCP_RELAY" "y" # busybox!
set_config_variable "BR2_PACKAGE_BIND" "y"
set_config_variable "BR2_PACKAGE_BIND_SERVER" "y"
set_config_variable "BR2_PACKAGE_BIND_TOOLS" "y"
set_config_variable "BR2_PACKAGE_RPCBIND" "y"
set_config_variable "BR2_PACKAGE_SOCAT" "y"
set_config_variable "BR2_PACKAGE_RSYNC" "y"
set_config_variable "BR2_PACKAGE_CURL" "y"
set_config_variable "BR2_PACKAGE_CURLFTPFS" "y"
set_config_variable "BR2_PACKAGE_SSHFS" "y"
set_config_variable "BR2_PACKAGE_STRACE" "y"
set_config_variable "BR2_PACKAGE_UEMACS" "y"
set_config_variable "BR2_PACKAGE_ETHTOOL" "y"
set_config_variable "BR2_PACKAGE_FILE" "y"
set_config_variable "BR2_PACKAGE_IPROUTE2" "y"
set_config_variable "BR2_PACKAGE_IPTABLES" "y"
set_config_variable "BR2_PACKAGE_KBD" "y"
set_config_variable "BR2_PACKAGE_LESS" "y"
set_config_variable "BR2_PACKAGE_LIGHTTPD" "y"
set_config_variable "BR2_PACKAGE_LIGHTTPD_OPENSSL" "y"
set_config_variable "BR2_PACKAGE_LIGHTTPD_BZIP2" "y"
set_config_variable "BR2_PACKAGE_LINKS" "y"
set_config_variable "BR2_PACKAGE_NANO" "y"
set_config_variable "BR2_PACKAGE_NANO_TINY" "y"
set_config_variable "BR2_PACKAGE_NCFTP" "y"
# set_config_variable "BR2_PACKAGE_NETCAT" "y" # busybox!
set_config_variable "BR2_PACKAGE_NMAP" "y"
set_config_variable "BR2_PACKAGE_NTP" "y"
# set_config_variable "BR2_PACKAGE_NTP_NTPD" "y" # busybox!
set_config_variable "BR2_PACKAGE_NTP_SNTP" "y"
set_config_variable "BR2_PACKAGE_NTP_NTPDATE" "y"
set_config_variable "BR2_PACKAGE_NTP_NTPDC" "y"
set_config_variable "BR2_PACKAGE_NTP_NTPQ" "y"
set_config_variable "BR2_PACKAGE_OPENSSH" "y"
set_config_variable "BR2_PACKAGE_OPENSSL" "y"
set_config_variable "BR2_PACKAGE_RADVD" "y"
set_config_variable "BR2_PACKAGE_READLINE" "y"
set_config_variable "BR2_PACKAGE_SUDO" "y"
set_config_variable "BR2_PACKAGE_TCPDUMP" "y"
# set_config_variable "BR2_PACKAGE_TFTPD" "y" # busybox!
# set_config_variable "BR2_PACKAGE_XZ" "y" # busybox!
set_config_variable "BR2_PACKAGE_WGET" "y"
set_config_variable "BR2_PACKAGE_ZLIB" "y"

# Basic essential settings:
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_INET_IPV6" "y"
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_INET_RPC" "y"
set_config_variable "BR2_ROOTFS_DEVICE_CREATION_STATIC" "y"
set_config_variable "BR2_INIT_BUSYBOX" "y"
set_config_variable "BR2_ROOTFS_SKELETON_DEFAULT" "y"
set_config_variable "BR2_TARGET_GENERIC_REMOUNT_ROOTFS_RW" "y"
set_config_variable "BR2_TARGET_ROOTFS_EXT2" "y"
set_config_variable "BR2_TARGET_ROOTFS_EXT2_BLOCKS" "0"
set_config_variable "BR2_TARGET_ROOTFS_EXT2_INODES" "0"
set_config_variable "BR2_TARGET_ROOTFS_EXT2_RESBLKS" "0"
set_config_variable "BR2_TARGET_ROOTFS_EXT2_NONE" "y"

# net-tools (in order to have `arp' and an ifconfig acceptiong CIDR notation):
# set_config_variable "BR2_PACKAGE_NET_TOOLS" "y" # busybox + our ifconfig wrapper!

# Option -q/--quagga
if [[ -n ${option_q} ]]; then

  # Package needed for quagga
  set_config_variable "BR2_PACKAGE_QUAGGA" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_ZEBRA" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_TCP_ZEBRA" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_BABELD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_BGPD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_BGP_ANNOUNCE" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_ISISD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_OSPFD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_OPAQUE_LSA" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_OSPF6D" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_RIPD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_RIPNGD" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_WATCHQUAGGA" "y"
  set_config_variable "BR2_PACKAGE_QUAGGA_SNMP" "y"
fi

# Tell Buildroot which kernel version we are supposing:
set_config_variable "BR2_KERNEL_HEADERS_VERSION" "y"
set_config_variable "BR2_DEFAULT_KERNEL_VERSION" '"'$KERNEL_VERSION'"'
set_config_variable "BR2_DEFAULT_KERNEL_HEADERS" '"'$KERNEL_VERSION'"'

#######################################
#  (Step 1) custom_packages_{yes,no}  #
#######################################

# Last-minute package's installation directives, using files
# _build.custom_packages_{no,yes} built and edited with specific
# `make' entries. Note that "yes" is prioritary over "no".
unset_tracing # otherwise too verbose!
CUSTOM_PACKAGES_NO="_build.custom_packages_no"
if [[ -f $CUSTOM_PACKAGES_NO ]]; then
  echo "Removing packages from uncommented lines of $CUSTOM_PACKAGES_NO"
  \grep -v "^#" $CUSTOM_PACKAGES_NO | \grep "BR2_PACKAGE_[A-Z0-9_][A-Z0-9_]*" | \
     while read PACKAGE; do
       set_config_variable "$PACKAGE" "n"
     done
fi
CUSTOM_PACKAGES_YES="_build.custom_packages_yes"
if [[ -f $CUSTOM_PACKAGES_YES ]]; then
  echo "Adding packages from uncommented lines of $CUSTOM_PACKAGES_YES"
  \grep -v "^#" $CUSTOM_PACKAGES_YES | \grep "BR2_PACKAGE_[A-Z0-9_][A-Z0-9_]*" | \
     while read PACKAGE; do
       set_config_variable "$PACKAGE" "y"
     done
fi
set_tracing # continue now in verbose mode

#######################################################
#  (Step 1) Last configurations on project_defconfig  #
#######################################################

# Bash will be (re-)selected anyway, even in a minimal setting:
set_config_variable "BR2_PACKAGE_BASH" "y"

# Toolchain settings: these three variables are initially set to "yes"
# in order to force Buildroot to consider all packages. Their value will
# be reconsidered in a second step:
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_LARGEFILE" "y"
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_WCHAR" "y"
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_CXX" "y"

########################################
#   (Step 1) Make project_defconfig    #
########################################

# Merge our minimal `project_defconfig' with
# Buildroot's defaults in order to generate $BUILDROOT/.config
once make -C $BUILDROOT project_defconfig


# =============================================================
#                 BUILDROOT CONFIGURATION (Step 2)
#                     working on `.config'
# =============================================================

# At this point we can forget the old configuration file used to
# build $BUILDROOT/.config with the previous `make' call. From
# this moment, we will work on $BUILDROOT/.config, so:
set_default_config_file $BUILDROOT/.config

# ___break_point___

# The `unset' function must act on the .config file generated by Buildroot,
# because Buildroot merges the `project_defconfig' with its own defaults.
unset_config_variable "BR2_LINUX_KERNEL" # ignore anyway the Buildroot's kernel compilation
unset_config_variable "BR2_TARGET_ROOTFS_TAR"
unset_config_variable "BR2_TARGET_ROOTFS_TAR_NONE"
unset_config_variable "BR2_TARGET_ROOTFS_TAR_OPTIONS"
set_config_variable "BR2_TAR_OPTIONS" '""'
set_config_variable "BR2_TARGET_ROOTFS_TAR" "n"

#######################################
#   (Step 2) TOOLCHAIN dependencies   #
#######################################

DEPENDENCIES=$(mktemp)
SELECTED_PACKAGES=$(awk <$BUILDROOT/.config -F "=" '$1 ~ /^BR2_PACKAGE_[A-Z][A-Z0-9_]*/ && $2 == "y" {print $1}')
for i in $SELECTED_PACKAGES; do
  j=${i#BR2_PACKAGE_}
  j=${j,,}
  CONFIG_IN=$BUILDROOT/package/$j/Config.in
  if [[ -f $CONFIG_IN ]]; then
    \grep -o 'depends on.*[!].*BR2_.*' $CONFIG_IN || true
  fi
done | tr ' ' '\n' | \grep -o "BR2_[A-Z][A-Z0-9_]*" | uniq | sort | uniq > $DEPENDENCIES
# Here $DEPENDENCIES contains a list like the following:
# BR2_INET_IPV6
# BR2_INSTALL_LIBSTDCPP
# BR2_LARGEFILE
# BR2_PACKAGE_QUAGGA
# BR2_PREFER_STATIC_LIB
# BR2_TOOLCHAIN_HAS_THREADS
# BR2_USE_WCHAR

function yes_or_no_according_to {
  if "$@"; then echo y; else echo n; fi
}

# In order to be able to select additional packages (nmap, wget):
# BR2_TOOLCHAIN_BUILDROOT_LARGEFILE is required for instance
# by BR2_PACKAGE_BIND but it is not automatically set by Buildroot, so:
y_or_n=$(yes_or_no_according_to grep -q "BR2_.*LARGEFILE" $DEPENDENCIES)
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_LARGEFILE" ${y_or_n}

# y_or_n=$(yes_or_no_according_to grep -q "BR2_.*WCHAR" $DEPENDENCIES)
# Forced because the version 3.2.44 of kernel's headers need this
# toolchain setting:
y_or_n=y
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_WCHAR" ${y_or_n}

y_or_n=$(yes_or_no_according_to grep -q "BR2_.*LIBSTDCPP" $DEPENDENCIES)
set_config_variable "BR2_TOOLCHAIN_BUILDROOT_CXX" ${y_or_n}

echo "TOOLCHAIN dependencies:"
tr '\n' ' ' <$DEPENDENCIES
rm $DEPENDENCIES

___break_point___

############################################
#   (Step 2) custom (interactive) running  #
############################################

# Custom? --custom -m
if [[ -n ${option_m} ]]; then
  PSEUDO_TERMINAL=$(tty)
  make -C $BUILDROOT menuconfig 0<$PSEUDO_TERMINAL 1>$PSEUDO_TERMINAL
fi

# =============================================================
#                   BUILDROOT COMPILATION
#                      (first round)
# =============================================================

# Compile all stuff a first time:
once make -C $BUILDROOT

# =============================================================
#                    BUSYBOX REBUILDING
#                      (second round)
# =============================================================

# We perform some settings directly into the Busybox's config file:
# Example: BUSYBOX_CONFIG=package/busybox/busybox-1.21.x.config
BUSYBOX_CONFIG=${BUILDROOT}/$(awk <$BUILDROOT/.config -F= '$1 == "BR2_PACKAGE_BUSYBOX_CONFIG" {print $2}' | tr -d '"')
set_config_variable "CONFIG_BRCTL" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_BRCTL_FANCY" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_BRCTL_SHOW" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_IPCALC" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_IPCALC_FANCY" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_IPCALC_LONG_OPTIONS" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_PGREP" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_PING6" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_PKILL" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_SPLIT" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_SPLIT_FANCY" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_STAT" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_STAT_FORMAT" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_SUM" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_TAC" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_UNCOMPRESS" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_UNEXPAND" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_FEATURE_UNEXPAND_LONG_OPTIONS" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_MKFS_EXT2" "y" $BUSYBOX_CONFIG
set_config_variable "CONFIG_PSTREE" "y" $BUSYBOX_CONFIG
# At this point $BUSYBOX_CONFIG is a merging of Builroot's settings
# with our settings.

# Now rebuild busybox with our specific settings:
BUSYBOX_BUILT_DIR=$(echo $BUILDROOT/output/build/busybox-*)
make -C $BUSYBOX_BUILT_DIR clean
sort_and_merge_config_files $BUSYBOX_CONFIG $BUSYBOX_BUILT_DIR/.config > $BUSYBOX_BUILT_DIR/.config.merged
cp $BUSYBOX_BUILT_DIR/{.config,.config.orig}
cp $BUSYBOX_BUILT_DIR/{.config.merged,.config}
# The second time we want execute the our filesystem tuning script:
set_config_variable "BR2_ROOTFS_POST_BUILD_SCRIPT" '"'$OUR_TUNING_SCRIPT'"' $BUILDROOT/.config

############################
#  (Second round) Compile  #
############################

# Note that the target `busybox-rebuild' doesn't have the expected behaviour:
once make -C $BUILDROOT busybox-reconfigure

####################################
#  (Second round) Store the image  #
####################################

# Move the image to $TWDIR:
FS_LOC=$TWDIR/$FS_NAME
mv $BUILDROOT/output/images/rootfs.ext2 $FS_LOC
cp $BUILDROOT/.config $TWDIR/buildroot.config
cp $BUSYBOX_BUILT_DIR/.config $TWDIR/busybox.config

# =============================================================
#                    GENERATING FILES
#                 {machine,router}-*.conf
# =============================================================

####################################
#  MD5SUM and other simple fields  #
####################################

MD5SUM=$(md5sum "$FS_LOC" | awk '{print $1}')
SUM=$(sum "$FS_LOC" | awk '{print $1}')
MTIME=$(stat -c "%Y" "$FS_LOC")
DATE=$(date +%Y-%m-%d)
AUTHOR=$(awk </etc/passwd -F : '($1 == "'$USER'") {print $5}' | awk -F , '{print $1}')

#######################
#  SUPPORTED_KERNELS  #
#######################

# Set SUPPORTED_KERNELS according to the compiled kernel:
if [[ -z $option_r ]]; then
# SUPPORTED_KERNELS='/.*-ghost/ ssl=xterm console=ttyS0'
  SUPPORTED_KERNELS="/$KERNEL_VERSION/" # /../ =>  ghost or not
else
  # With option --router the console must be 'none' except when the user
  # explicitely requires a unix terminal:
  SUPPORTED_KERNELS="/$KERNEL_VERSION/" # /../ =>  ghost or not
fi

##############################################
#  X11_SUPPORT and memory-related variables  #
##############################################

function set_X11_SUPPORT_and_related_variables_according_to_choosed_packages {
  # global X11_SUPPORT BUILDROOT
  local CONFIG_FILE=${1:-$BUILDROOT/.config}
  local X11_RELATED_PACKAGES
  # ---
  if grep -q "BR2_PACKAGE_XLIB_LIBX11=y" "$CONFIG_FILE"; then
     if grep -q "BR2_PACKAGE_XSERVER_XORG_SERVER=y" "$CONFIG_FILE"; then
       X11_SUPPORT="xnested"
       MEMORY_MIN_SIZE=32
       MEMORY_SUGGESTED_SIZE=48
     else
       X11_SUPPORT="xhosted"
       MEMORY_MIN_SIZE=24
       MEMORY_SUGGESTED_SIZE=48
     fi
  else
    X11_SUPPORT="none"
    MEMORY_MIN_SIZE=16 # tested
    MEMORY_SUGGESTED_SIZE=24
  fi
}

# Launch the function:
set_X11_SUPPORT_and_related_variables_according_to_choosed_packages "$BUILDROOT/.config"

###################
#   BINARY_LIST   #
###################

# Looking for binaries in $BUILDROOT/output/target
TARGET_DIR=$BUILDROOT/output/target
pushd $TARGET_DIR
BIN_OR_SBIN_DIRS=$(find . -type d \( -name "bin" -o -name "sbin" \) )
BINARY_LIST=$(find $BIN_OR_SBIN_DIRS -perm -u=x ! -type d ! -name "*[.]so*" -exec basename {} \; | sort)
# Some binaries like '[' or '[[' will provoke some problems applying `sed' or `awk' (see above), so:
BINARY_LIST=$(echo $BINARY_LIST | tr ' ' '\n' | \grep "[a-zA-Z][a-zA-Z_.]*")
BINARY_LIST=$(echo $BINARY_LIST)
popd

#######################
#   FILLING TEMPLATE  #
#######################

cp ../../share/filesystems/machine-template.conf $FS_LOC.conf

# Using `sed' for simple replacements:
sed -e "s/^MD5SUM=.*/MD5SUM=$MD5SUM/"  \
    -e "s/^SUM=.*/SUM=$SUM/"           \
    -e "s/^MTIME=.*/MTIME=$MTIME/"     \
    -e "s/^DATE=.*/DATE=$DATE/"        \
    -e "s/^AUTHOR=.*/AUTHOR=\"$AUTHOR\"/"  \
    -e "s/^X11_SUPPORT=.*/X11_SUPPORT=\"$X11_SUPPORT\"/"  \
    -e "s/^MEMORY_MIN_SIZE=.*/MEMORY_MIN_SIZE=$MEMORY_MIN_SIZE/"        \
    -e "s/^MEMORY_SUGGESTED_SIZE=.*/MEMORY_SUGGESTED_SIZE=$MEMORY_SUGGESTED_SIZE/"        \
    -i ${FS_LOC}.conf

# Using `user_config_set' for replacements involving variables
# bound to values with special characters (as '/') and/or multiple lines.
user_config_set "BINARY_LIST"       "=" "'$BINARY_LIST'"       ${FS_LOC}.conf
user_config_set "SUPPORTED_KERNELS" "=" "'$SUPPORTED_KERNELS'" ${FS_LOC}.conf

# Rename the built filesystem and its .conf file simply adding the suffix $SUM:
mv $FS_LOC ${FS_LOC}-${SUM}
mv $FS_LOC.conf ${FS_LOC}-${SUM}.conf


# =============================================================
#             ROUTER LINK AND OTHER FINAL ACTIONS
# =============================================================

pushd "$(dirname $FS_LOC)"
if [[ -n $option_r ]]; then
 FS_BASENAME="$(basename ${FS_LOC}-${SUM})"
 ROUTER_FS_BASENAME=router-${FS_BASENAME#machine-}
 cp $FS_BASENAME.conf $ROUTER_FS_BASENAME.conf
 ln -s $FS_BASENAME $ROUTER_FS_BASENAME
 cat >INSTALL <<EOF
# In order to preserve MTIME and symlink, type:
cp -a {machine,router}-* /usr/local/share/marionnet/filesystems/
EOF
else
 cat >INSTALL <<EOF
# In order to preserve MTIME, type:
cp -a machine-* /usr/local/share/marionnet/filesystems/
EOF
fi
popd

# =============================================================
#                         KERNEL
# =============================================================

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
    download_patch_and_compile_kernel $KERNEL_VERSION $TWDIR
    # Move the whole directory to the good place (../pupisto.kernel/)
    # in order to potentially share it among other filesystem building:
    BUILT_DIR=_build.linux-${KERNEL_VERSION}.$(date +%Y-%m-%d.%H\h%M).$RANDOM
    echo "Moving \`$TWDIR/linux-$KERNEL_VERSION' -> \`../pupisto.kernel/$BUILT_DIR'"
    mv $TWDIR/linux-$KERNEL_VERSION ../pupisto.kernel/$BUILT_DIR
    ln -s ../../pupisto.kernel/$BUILT_DIR $TWDIR/linux-$KERNEL_VERSION
  fi
fi

# =============================================================
#                         GREETINGS
# =============================================================

# Store the log file into the output directory:
make_a_human_readable_log_into_working_directory

[[ -f $CUSTOM_PACKAGES_NO  ]] && mv $CUSTOM_PACKAGES_NO  $TWDIR/
[[ -f $CUSTOM_PACKAGES_YES ]] && mv $CUSTOM_PACKAGES_YES $TWDIR/

echo "---"
ls -ld $TWDIR
echo "---"
echo "Pay attention to move (or copy with option \`-a') the filesystem in order to preserve the MTIME."
echo "If something goes wrong installing your filesystem, you can restore the correct"
echo "MTIME with the following command:"
echo "sudo touch -d \$(date -d '@$MTIME') $FS_NAME"
echo
echo "Success."
