#!/bin/bash

# Author: Luca Saiu 
# Date: 2007
# Modified (minor changes) by Jean-Vincent Loddo (2013)
# Licence GPL

# Read the kernel command line variable 'hostname'; this crude hack is
# needed because filesystems are mounted read-only at this stage.
# The following command became something like:
# export hostname=m1
export $(tr ' ' '\n' </proc/cmdline | grep "^hostname=") &>/dev/null

# Show a first message in the terminal window title bar...
echo -en '\033]0;'Booting up the virtual machine \"$hostname\"...'\a'

