#!/bin/bash
# (file to be sourced)

# This file is part of marionnet
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

# =============================================================
#                    GENERATING FILES
#                 {machine,router}-*.conf
# =============================================================

function rename_with_sum_and_make_image_dot_conf {
 # global FS_NAME (output)
 local $FS_LOC="$1"
 # Checking parameters and calling context:
 [[ -f "$FS_LOC" ]] || return 2
 # This image-dependent function must be provided independently:
 type -p set_X11_SUPPORT_and_related_variables_according_to_choosed_packages || return 3
 # The following global variables must be set:
 [[ -n $KERNEL_VERSION && -n $BINARY_LIST ]] || return 4
 # The following template must be available:
 [[ -f ../../share/filesystems/machine-template.conf ]] || return 5
 # ---
 
 # MD5SUM and other simple fields
 local MD5SUM=$(md5sum "$FS_LOC" | awk '{print $1}')
 local SUM=$(sum "$FS_LOC" | awk '{print $1}')
 local MTIME=$(stat -c "%Y" "$FS_LOC")
 local DATE=$(date +%Y-%m-%d)
 local AUTHOR=$(awk </etc/passwd -F : '($1 == "'$USER'") {print $5}' | awk -F , '{print $1}')

 # Set SUPPORTED_KERNELS according to the compiled kernel:
 SUPPORTED_KERNELS="/$KERNEL_VERSION/" # /../ =>  ghost or not

 # X11_SUPPORT and memory-related variables
 set_X11_SUPPORT_and_related_variables_according_to_choosed_packages
 [[ -n $X11_SUPPORT && -n $MEMORY_MIN_SIZE && -n $MEMORY_SUGGESTED_SIZE ]] || return 6

 # FILLING TEMPLATE
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
 user_config_set "SUPPORTED_KERNELS" "=" "'$SUPPORTED_KERNELS'" ${FS_LOC}.conf || true

 # Rename the built filesystem and its .conf file simply adding the suffix $SUM:
 mv $FS_LOC ${FS_LOC}-${SUM}
 mv $FS_LOC.conf ${FS_LOC}-${SUM}.conf
 export FS_NAME=${FS_LOC}-${SUM}
 return 0
 
} # rename_with_sum_and_make_image_dot_conf


# Automatically export previously defined functions:
export -f $(awk '/^function/ {print $2}' ${BASH_SOURCE[0]})
