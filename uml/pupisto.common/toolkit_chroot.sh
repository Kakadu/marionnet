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
#                 ENHANCED VERSION OF chroot
# =============================================================

# In order to prevent annoying messages related to locales:
GLOBAL_LOCALE="en_US.utf8"
shopt -s expand_aliases
alias chroot='LANG='$GLOBAL_LOCALE' LC_ALL=$LANG LC_MESSAGES=$LANG LANGUAGE=$LANG chroot'

# Create a temporary file in /tmp/ starting with "$1" and followed by
# the current timestamp:
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
 local APPEND
 local FOLLOW
 if [[ "$1" = "-a" || "$1" = "--append" ]]; then
   APPEND="-a"
   shift
 fi
 if [[ "$1" = "-f" || "$1" = "--follow" ]]; then
   FOLLOW=y
   shift
 fi
 [[ $# -ge 3 && ( -w "$1" || ! -e "$1" ) && "$2" = "with" ]] || {
   echo "Usage: rewrite FILE with COMMAND"
   echo "where COMMAND refers to FILE (as argument or standard input)"
   return 2
   }
 local TARGET="$1"
 shift 2
 local TMPFILE=$(mktemp)
 local CODE=0
 if [[ -z $FOLLOW ]]; then
   if [[ -z $APPEND ]]; then
     "$@" > $TMPFILE
     CODE=$?
   else
     "$@" >> $TMPFILE
     CODE=$?
   fi
 else
     "$@" | tee $APPEND $TMPFILE
     CODE=$?
 fi
 cat $TMPFILE > $TARGET
 rm -f $TMPFILE
 return $CODE
}

# A straightforward alternative to `sudo_fcall rewrite'.
# Note that it's important to leave the sudo stdin empty
# in order to prevent `sudo' from asking the password every time.
#
# Usage: sudo_fprintf [-a|--append] [-m|--mode|--chmod MODE] FILE FORMAT [ARGUMENT]...
function sudo_fprintf {
  local APPEND
  if [[ $1 = "-a" || $1 = "--append" ]]; then
    APPEND="y"
    shift
  fi
  # ---
  local MODALITIES
  if [[ $1 = "-m" || $1 = "--mode" || $1 = "--chmod" ]]; then
    MODALITIES="$2"
    shift 2 || return 1
  fi
  # ---
  local TARGET="$1"
  shift
  # ---
  local TMPFILE=$(mktemp)
  if [[ -n $APPEND && -f $TARGET ]]; then
    sudo cp "$TARGET" "$TMPFILE"
  fi
  printf "$@" >>$TMPFILE && sudo cp $TMPFILE "$TARGET"
  if [[ -n $MODALITIES ]]; then sudo chmod $MODALITIES $TARGET; fi
  rm $TMPFILE
}

# Return the list of pids still rooted in $1
function pidsrooted {
 local ROOT=$(realpath "$1")
 local i
 # The command `find' below is too noisy:
 type pause_tracing &>/dev/null && pause_tracing
 for i in $(find /proc -maxdepth 1 -noleaf -name "[1-9]*"); do
   echo -n "$i "; readlink $i/root;
 done | cut -c7- | \grep $ROOT | cut -d" " -f1 | tac | tr '\n' ' '
 type continue_tracing &>/dev/null && continue_tracing
}

# Return the list of fs mounted in $1 (except /proc and /sys)
function fsmounted {
 local ROOT=$(realpath "$1")
 cat /proc/mounts | \grep "$ROOT/" | awk '{print $2}' | \grep -v "$ROOT/proc" | \grep -v "$ROOT/sys" | tac | tr '\n' ' '
}

# Usage: TICKET=$(save_files XXX YYY ... ZZZ)
function save_files {
  local ARCHIVE=$(mktemp)
  tar -czf $ARCHIVE "$@"
  echo "$PWD:$ARCHIVE"
}

# Usage: restore_files $TICKET
# where TICKET has been provided by `save_files'
function restore_files {
  local OLD_PWD ARCHIVE
  IFS=: read OLD_PWD ARCHIVE <<<"$1"
  tar -C $OLD_PWD -xzf $ARCHIVE && rm -f $ARCHIVE
}

# Usage: sudo_fcall FUNCTION ACTUALS..
# Limitations: the called function can call itself only the exported functions;
# you can show these functions with `export -fp'.
function sudo_fcall {
 # global COOL_SUDO
 [[ $# -ge 1 ]] || return 2
 local FUNC="$1"
 if [[ -z $COOL_SUDO ]]; then
   COOL_SUDO=$(mktemp /tmp/COOL_SUDO.XXXXXX)
   export COOL_SUDO
   chmod +x $COOL_SUDO
 fi
 # ---
 echo '#!/bin/bash'       > $COOL_SUDO
 # Put the definition of all exported functions:
 export -pf              >> $COOL_SUDO
 # ---
 # Put the definition of all exported variables:
 {
 echo 'PATH_BACKUP=$PATH';        # save the current root's setting
 export -p;
 echo 'PATH=$PATH_BACKUP:$PATH';  # restore the root's setting;
 } >> $COOL_SUDO
 # ---
 # Put all current set-options (-e, -x, ..):
 echo "set -$-";
 # ---
 # Put the definition of the called function:
 type $FUNC | tail -n +2 >> $COOL_SUDO
 # ---
 # Tracing:
 if [[ $BASH_XTRACING = y ]]; then
   echo "PS4='+ [$COOL_SUDO] $PS4'" >> $COOL_SUDO
 fi
 # ---
 # Put now the command that we want to execute as root:
 echo "$@" >> $COOL_SUDO
 # ---
 # Finally call the script with sudo:
 sudo $COOL_SUDO
}

# Enhanced chroot: imports caller's system configurations (network,X,..)
# and exits very cleanly (killing, unmounting,..).
# Usage: sudo_fcall careful_chroot ...
function careful_chroot {
 # global GLOBAL_LOCALE
 [[ $# -ge 1 ]] || return 2
 local ROOT=$(realpath "$1")
 shift
 [[ -d $ROOT && -x $ROOT ]] || return 1
 test $(id -u) -eq 0 || {
   echo 1>&2 "You must be root to call this function"; return 3;
   }
 # ---
 local i

 # Go to the target, but not in a chrooted environnement:
 pushd "$ROOT" 1>&2 2>/dev/null

 # Manage the /dev/null problem
 local DEV=$(df "$ROOT" | grep "^/dev" | cut -f1 -d" ")
 if [[ -n $DEV ]]; then
   mount -o remount,dev $DEV
 else
   # May be an aufs?
   DEV=$(df . | tail -n 1 | cut -f1 -d" ")
   if [[ "$DEV" = aufs ]]; then
     : # Do nothing
   else
     # Probably a loopback, try to remount with the dev option
     mount -o remount,dev . || true
   fi
 fi

 # Mount /proc and /sys
 local LEAVE_PROC_MOUNTED LEAVE_SYS_MOUNTED
 mount -t proc  proc  ./proc || LEAVE_PROC_MOUNTED=y
 mount -t sysfs sysfs ./sys  || LEAVE_SYS_MOUNTED=y

 # Save relevant files
 local TICKET=$(save_files "etc/resolv.conf" "root/.bashrc" "etc/fstab")
 # Copy relevant files from current root:
 local TFILES="etc/resolv.conf"
 for i in $TFILES; do rm -f $i; cat /$i > $i; done

 # X server
 type -P xhost &>/dev/null && xhost 1>&2 + localhost
 echo "export DISPLAY=${DISPLAY:-localhost:0.0}" >> root/.bashrc

 # clear fstab
 >etc/fstab

 # Go:
 local L="${GLOBAL_LOCALE:-en_US.utf8}"
 LANG=$L LC_ALL=$L LC_MESSAGES=$L LANGUAGE=$L chroot $PWD "$@"
 local RETURN_CODE=$?
 sync

 # Restore previously saved files:
 restore_files $TICKET

 # Clean history
 echo 1>&2 "Cleaning history..."
 # Note that the following setting is not persistent, because this
 # function will be executed by a distinct Bash interpreter (called
 # by the wrapper `sudo_fcall'):
 shopt -s nullglob
 for i in "$PWD"/{root/,home/*/}.bash_history; do >$i; done

 # Kills all processes rooted in the previous root
 ROOT=$(realpath $PWD)
 local LIST=$(pidsrooted $PWD)
 echo 1>&2 -n "Killing all processes rooted here (${LIST% *}) ..."
 chroot "$ROOT" bash -c "for i in $LIST; do kill -15 \$i && sleep 1s; done" 2>/dev/null || true
 sleep 1s
 chroot "$ROOT" bash -c "for i in $LIST; do kill  -9 \$i && sleep 1s; done" 2>/dev/null || true
 echo 1>&2 " done."

 # Warning for processes still running
 LIST=$(pidsrooted $PWD)
 [[ -z $LIST ]] || {
   echo 1>&2 "WARNING: the following list of processes still running with root=$ROOT"
   local NAME PID
   for i in $LIST; do
    NAME=$(grep '^Name:' /proc/$i/status)
    PID=$(grep '^Pid:'   /proc/$i/status)
    echo 1>&2 -e "$NAME\t($PID)"
   done
   }

 # Umount all but /proc and /sys
 LIST=$(fsmounted $PWD)
 for i in $LIST; do umount $i 2>/dev/null; done

 # Warning for fs still mounted
 LIST=$(fsmounted $PWD)
 if [[ -z $LIST ]]; then
   # Finally umount /proc and /sys
   [[ $LEAVE_PROC_MOUNTED = y ]] || umount ./proc
   [[ $LEAVE_SYS_MOUNTED  = y ]] || umount ./sys
 else
   echo 1>&2 "WARNING: the following list of filesystems are still mounted in $ROOT"
   cat /proc/mounts | \grep "$ROOT/" 1>&2
 fi

 popd 1>&2
 return $RETURN_CODE
}

# Simply an shorthand to `sudo_fcall careful_chroot':
function sudo_careful_chroot {
  sudo_fcall careful_chroot "$@"
}

# Copy with tar the content of directory into another (existing or not).
# Usage: copy_content_into_directory ORIGDIR [DESTDIR]
# By default DESTDIR=.
function copy_content_into_directory {
 [[ $# -ge 1 && -e "$1" ]] || return 2
 local ORIG="$1";
 local ORIGDIR ORIGNAME
 if [[ -d $ORIG ]]; then
   ORIGDIR="$ORIG";
   ORIGNAME=./
 else
   ORIGDIR=$(dirname "$ORIG");
   ORIGNAME=$(basename "$ORIG");
 fi
 local DESTDIR="${2:-$PWD}";
 [[ -d $DESTDIR ]] || mkdir -p $DESTDIR
 if [[ $(realpath $ORIGDIR) = $(realpath $DESTDIR) ]]; then
   echo "Sorry, same origin and destination directory ($(realpath $DESTDIR))";
   return 1
 fi
 local R
 # In any case, don't stop the execution in case of error:
 if tar -C "$ORIGDIR" -cf - -- "$ORIGNAME" | tar -C "$DESTDIR" -xf -; then R=0; else R=$?; fi
 return $R
}

function binary_list {
 local i DIRS BINARY_LIST
 DIRS=$(for i in ${PATH//:/ }; do [[ -d $i ]] && echo $i; done)
 find $DIRS -perm -u=x ! -type d ! -name "*[.]so*" -exec basename {} \; | sort | tr '\n' ' '
}

function make_shellshock_somewhere {
 # global SHELLSHOCK
 [[ -x $SHELLSHOCK ]] && return 0
 # else continue:
 SHELLSHOCK=$(mktemp "/tmp/shellshock.XXXXXX.py")
 cat >$SHELLSHOCK <<EOF
#!/usr/bin/python

# Run a program (probably an old version of Bash) in a modified environment.
# The old (and unsafe) convention for function names is resumed. 
# See http://www.dwheeler.com/essays/shellshock.html
# J.V. Loddo - GPL

# Usage: shellshock.py [COMMAND [ARG]...]

import os
import sys

result={}
#---
for k in os.environ:
  if (k[:10] == 'BASH_FUNC_') and ((k[-2:] == '%%') or (k[-2:] == '()')):
    v = os.environ[k]
    func_name = k[10:-2]
    # We provide the three possibilities!
    result[func_name] = v
    if (k[-2:] == '%%'): 
      result['BASH_FUNC_'+func_name+'()'] = v
    else:
      result['BASH_FUNC_'+func_name+'%%'] = v
#---

# Add new bindings:
for k in result:
  os.environ[k]=result[k]

# Exec the program:
os.execvp(sys.argv[1], sys.argv[1:]) 
EOF
# ---
chmod +x $SHELLSHOCK
}

function chroot_is_shellshock_need {
 local ROOT="$1"
 # ---
 if strings "$ROOT/bin/bash" | grep -q "BASH_FUNC_"; then
   local BASH_VERSION=$(strings "$ROOT/bin/bash" | grep "Bash version" | grep -o "[1-9][0-9]*[.][1-9][0-9]*[.][1-9][0-9]")
   [[ $BASH_VERSION < "4.3" ]] && return 0
   { strings "$ROOT/bin/bash" | grep -q "%%"; } && return 0
   return 1
 else 
   return 0 # need!
 fi
}

function chroot_fcall {
 local ROOT="$1"
 shift
 local CMD="$@"  # `bash -c' needs a single arguments
 # ---
 # Ensure Bash's compatibility about function's name exporting:
 if chroot_is_shellshock_need; then
   make_shellshock_somewhere
   $SHELLSHOCK chroot "$ROOT" bash -c "$CMD"
 else  
   chroot "$ROOT" bash -c "$CMD"
 fi
}

# Example: sudo_chroot_fcall $ROOT binary_list
function sudo_chroot_fcall {
 sudo_fcall chroot_fcall "$@"
}

function sudo_chroot_binary_list {
 local ROOT="$1"
 sudo_chroot_fcall "$ROOT" binary_list
}


# Automatically export previously defined functions:
export -f $(awk '/^function/ {print $2}' ${BASH_SOURCE[0]})
