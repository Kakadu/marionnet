#!/bin/bash

# This file is part of Marionnet, a virtual network laboratory
# Copyright (C) 2007  Jean-Vincent Loddo
# Copyright (C) 2007  Luca Saiu

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
DIR=${1:-$PWD}

[[ -d "$DIR" ]] || {
 echo "Directory doesn't exist. Exiting."
 exit 3
} >&2

# The device containing the directory
DEV=$(df -P "$DIR" | tail -n -1 | grep "^[a-zA-Z/0-9]" | awk '{print $1}')

# The filesystem type of the device
FSTYPE=$(mount -l | grep "^${DEV} " | awk '{print $5}' | head -n 1)

[[ -n "$FSTYPE" ]] || {
 echo "Cannot determine the filesystem type. Exiting."
 exit 2
} >&2

# Check if the filesystem type belongs the white list.
# Note that apparently xfs no longer supports sparse files in Ubuntu 12.04 (kernel 3.2).
WHITE_LIST="reiserfs reiser4 ext4 ext4dev ext3 ext2 udf ntfs jfs ufs vxfs xiafs"
echo "$WHITE_LIST" | grep -qw "$FSTYPE"

exit $?
