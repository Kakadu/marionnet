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


# When tracing, print the line number and current stack of function calls:
PS4='+ [#${LINENO}$(A=${FUNCNAME[@]}; A=${A% main}; A=${A// /\|}; [[ -n $A ]] && echo " ${A}")] '

# =============================================================
#                     REMEMBER TRACING ACTIONS
#                          (set [-+]x)
# =============================================================

function set_tracing {
 # global BASH_XTRACING
 export BASH_XTRACING=y
 set -x
}

function unset_tracing {
 # global BASH_XTRACING
 set +x
 unset BASH_XTRACING
}

function pause_tracing {
 # global BASH_XTRACING  BASH_XTRACING_PAUSE
 if [[ $BASH_XTRACING = y ]]; then
   export BASH_XTRACING_PAUSE=y
   unset_tracing
 fi
}

function continue_tracing {
 # global BASH_XTRACING  BASH_XTRACING_PAUSE
 if [[ $BASH_XTRACING_PAUSE = y ]]; then
   unset BASH_XTRACING_PAUSE
   set_tracing
 fi
}

# =============================================================
#                       BREAK POINTS
# =============================================================

function set_debugging {
 # global DEBUGGING_MODE
 DEBUGGING_MODE=y
}

function unset_debugging {
 # global DEBUGGING_MODE
 unset DEBUGGING_MODE
}

# Set a break point for debugging. When a break point is reached
# in "debugging" mode, a bash shell is launched in order to allow
# to inspect current variables or file contents.
#
# Usage:
# source toolkit_debugging.sh
# set_debugging
# ...
# ___break_point___
# ...
# ___break_point___
# ...
function ___break_point___ {
 # global BASH_XTRACING DEBUGGING_MODE BREAK_POINT_NO
 local restore_tracing
 if [[ -n $BASH_XTRACING ]]; then
   unset_tracing
   restore_tracing=y
 fi
 let BREAK_POINT_NO=BREAK_POINT_NO+1
 # Ignore if we are not in debugging mode:
 [[ $DEBUGGING_MODE = "y" ]] || return 0
 local f fl v
 # Export all defined UPPERCASE variables:
 v=$(\grep -o '[A-Z][A-Z_0-9]*=' $0 | awk -F= '{print $1}' | uniq | sort | uniq | tr '\n' ' ')
 export $v
 # Export all defined functions (defined with the syntax "function foo {..}"):
 for f in $(awk '/^[ ]*function/ {print $2}' $0); do
   { type &>/dev/null $f && export -f $f && fl+="$f\n"; } || true
 done
 echo "--- Break point"
 echo "--- Variables:"
 echo "${v// /  }" | fmt -w 80
 echo "--- Functions:"
 echo "$(echo -e $fl | sort | tr '\n' ' ')" | fmt -w 80
 echo "---"
 echo "--- Bash subshell launched for debugging: (exit with CTRL-D)"
 echo "---"
 PS1='--- [BREAK-POINT-'$BREAK_POINT_NO'][$? \W]\\$ ' bash --noprofile --norc 0<$(tty) 1>$(tty) 2>$(tty)
 if [[ -n $restore_tracing ]]; then
   set_tracing
 fi
}


function set_once_actions_file {
 # global ONCE_ACTIONS_FILE
 ONCE_ACTIONS_FILE="$1"
 echo 1>&2 "Once actions file set to \`$ONCE_ACTIONS_FILE'"
}

function make_temporary_once_actions_file {
 # global ONCE_ACTIONS_FILE
 set_once_actions_file "$(mktemp /tmp/$(basename $0).once_actions_file.XXXXXX)"
}

# Usage: once [-r/--register-anyway] <COMMAND>
# Register successfully executed commands in order to prevent to repeat their execution.
function once {
 local REGISTER_ANYWAY
 if [[ $1 = "--register-anyway" || $1 = "-r" ]]; then
   REGISTER_ANYWAY=y
   shift
 fi
 # global ONCE_ACTIONS_FILE
 [[ -n "$ONCE_ACTIONS_FILE" ]] || make_temporary_once_actions_file
 >>"$ONCE_ACTIONS_FILE"
 # We define variable with unusual names in order to prevent us to
 # hide some environment variable with these names.
 local ___CODE___=0
 local ___POINT___
 ___POINT___=$(echo "${FUNCNAME[@]} ### ${BASH_SOURCE[@]} ### ""$@")
 if grep -q "^${___POINT___}$" "$ONCE_ACTIONS_FILE"; then
   echo "Already done, skipping."
 else
   "$@" || ___CODE___=$?
   if [[ -n $REGISTER_ANYWAY || ${___CODE___} -eq 0 ]]; then
     echo "${___POINT___}" >> $ONCE_ACTIONS_FILE
   fi
 fi
 return ${___CODE___}
}

function exiting_because_error {
 echo -e "Exiting because of an unexpected error in line $BASH_LINENO"
 exit 3
}
# Trap errors:
trap exiting_because_error ERR

# Automatically export previously defined functions:
export -f $(awk '/^function/ {print $2}' ${BASH_SOURCE[0]})
