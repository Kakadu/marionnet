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
#                  TABULAR FILES UPDATES
# =============================================================

# Example:
# tabular_file_update -i -d ":" -k 1 --key-value "root" -f 7 --field-value "/bin/bash" --field-old-value "/bin/sh" etc/passwd
# Returns with following codes:
# 0 => successfully finished and file updated
# 1 => successfully finished but file unchanged (neutralized by option --ignore-unchanged)
# 2 => failed somewhere
function tabular_file_update {
 local SEP KEY KEY_VALUE FIELD_NO FIELD_NEW_VALUE FIELD_OLD_VALUE FIELD_OLD_REGEXP
 local EDIT_IN_PLACE BACKUP IGNORE_BLANKS IGNORE_UNCHANGED
 local RETURN_CODE
 local BOOLEAN_AND=1
 # Defaults
 KEY=1
 FIELD_OLD_REGEXP=""
 # Parsing actuals:
 while [[ -${1#-} = $1 ]]; do
   case "$1" in
    -d|-F|--field-separator) SEP="$2"; shift 2 ;;
                -k|--key-no) KEY="$2"; shift 2 ;;
            -kv|--key-value) KEY_VALUE="$2"; shift 2 ;;
              -f|--field-no) FIELD_NO="$2"; shift 2 ;;
          -fv|--field-value) FIELD_NEW_VALUE="$2"; shift 2 ;;
     -fov|--field-old-value) FIELD_OLD_VALUE="$2"; shift 2 ;;
    -for|--field-old-regexp) FIELD_OLD_REGEXP="$2"; shift 2 ;;
            --ignore-blanks) IGNORE_BLANKS=y; shift ;;
         --ignore-unchanged) IGNORE_UNCHANGED=y; shift ;;
              -i|--in-place) EDIT_IN_PLACE=y; shift ;;
                -b|--backup) BACKUP=y; shift ;;
                          *) echo "Unknown option: $1"; return 2 ;;
   esac
 done

 local FS OFS
 if [[ -z "$SEP" ]]; then
   # Awk defaults:
   FS="[ \t][ \t]*"
   OFS=" "
 elif [[ -z "$IGNORE_BLANKS" ]]; then
   FS="$SEP"
   OFS="$SEP"
 else
   FS="[ \t]*${SEP}[ \t]*"
   OFS="$SEP"
 fi
 [[ -n "$KEY_VALUE" ]] || { echo "Wrong arguments: --key-value must be specified" ; return 2; }
 FIELD_NO=${FIELD_NO:-$KEY}
 if [[ -z "$FIELD_OLD_VALUE" && "$KEY" = "$FIELD_NO" ]]; then
   FIELD_OLD_VALUE=$KEY_VALUE
 fi

 local TMPFILE=$(mktemp)
 for FILE in "$@"; do
 if [[ -n "$FIELD_OLD_VALUE" ]]; then
  awk <"$FILE" >"$TMPFILE" \
   -F "$FS" -v OFS="$OFS" \
   -v KEY="$KEY" \
   -v KEY_VALUE="$KEY_VALUE" \
   -v FIELD_NO=$FIELD_NO \
   -v FIELD_NEW_VALUE="$FIELD_NEW_VALUE" \
   -v FIELD_OLD_VALUE="$FIELD_OLD_VALUE" \
   -v FIELD_OLD_REGEXP="$FIELD_OLD_REGEXP" \
   '($KEY == KEY_VALUE) && ($FIELD_NO == FIELD_OLD_VALUE) && ($FIELD_NO ~ FIELD_OLD_REGEXP) {
       for (i=1; i<NF; i++) {
         if (i == FIELD_NO) printf("%s%s",FIELD_NEW_VALUE,OFS);
         else printf("%s%s",$i,OFS);
         }
       if (NF == FIELD_NO) printf("%s\n",FIELD_NEW_VALUE);
       else printf("%s\n",$NF);
       next;
       }
   {print}'
 else
  awk <"$FILE" >$TMPFILE \
   -F "$FS" -v OFS="$OFS" \
   -v KEY="$KEY" \
   -v KEY_VALUE="$KEY_VALUE" \
   -v FIELD_NO=$FIELD_NO \
   -v FIELD_NEW_VALUE="$FIELD_NEW_VALUE" \
   -v FIELD_OLD_REGEXP="$FIELD_OLD_REGEXP" \
   '($KEY == KEY_VALUE) && ($FIELD_NO ~ FIELD_OLD_REGEXP) {
       for (i=1; i<NF; i++) {
         if (i == FIELD_NO) printf("%s%s",FIELD_NEW_VALUE,OFS);
         else printf("%s%s",$i,OFS);
         }
       if (NF == FIELD_NO) printf("%s\n",FIELD_NEW_VALUE);
       else printf("%s\n",$NF);
       next;
       }
   {print}'
 fi || return 2

 if [[ -z $IGNORE_UNCHANGED ]] && diff 1>/dev/null -q "$FILE" $TMPFILE; then
   RETURN_CODE=1 # no update!
 else
   RETURN_CODE=0 # update occurred (or ignore the question if --ignore-unchanged is set)
 fi
 let BOOLEAN_AND="BOOLEAN_AND*RETURN_CODE" || true

 # An update occurred:
 if [[ $EDIT_IN_PLACE = "y"  && $RETURN_CODE = 0 ]]; then
   if [[ $BACKUP = "y" ]]; then
     cp -v "$FILE" "$FILE.backup" --backup="numbered" 1>&2
   fi
   # Now rewrite FILE:
   cat "$TMPFILE" > "$FILE"
 elif [[ $EDIT_IN_PLACE = "y" && $RETURN_CODE = 1 ]]; then
   :
 else
   cat "$TMPFILE"
 fi || return 2

 done
 rm $TMPFILE
 # Return 0 if at least one update occurred:
 return $BOOLEAN_AND
}

# Build the `grep' regexp that exactly matches the given string:
function quoting_for_grep {
 sed <<<"$1" \
   -e 's/\[/\\\[/g'  \
   -e 's/\]/\\\]/g'  \
   -e 's/\./\\\./g'  \
   -e 's/[*]/\\*/g'  \
   -e 's/[|]/\\|/g'  \
   -e 's/^\^/\\\^/g' \
   -e 's/\$$/\\\$/g'
}

# Example:
# test_quoting_for_grep '^[2.6.18-(ghost)] | con=none^ ssl=$xterm console=ttyS0$'
# returns with 0
function test_quoting_for_grep {
 \grep "$(quoting_for_grep "$1")" <<<"$1"
 # must return 0 everytime!
}

# =============================================================
#             USER-COMPLIANT CONFIGURATION FILES
# =============================================================

# Set a configuration binding (<KEY>,<VALUE>) updating a line if possible,
# or removing (and updating) a commented line if it exists or, as last
# attempt, appending a row in the form "<KEY><SEP><VALUE>" in the specified
# file(s).
#
# Usage: user_config_set <KEY_VALUE> <DELIMITER> <FIELD_VALUE> [<FILE>]..
#
# Examples:
# user_config_set "PermitRootLogin" " " "yes" /etc/sshd_config
#
# Returns with following codes:
# 0 => successfully finished and file updated (or --ignore-unchanged is set)
# 1 => successfully finished but file unchanged (and --ignore-unchanged is not set)
# 2 => failed somewhere
function user_config_set {
  local RETURN_CODE_WHEN_UNCHANGED=1 # by default
  local IGNORE_UNCHANGED
  # ---
  if [[ $1 = "--ignore-unchanged" ]]; then
    RETURN_CODE_WHEN_UNCHANGED=0
    IGNORE_UNCHANGED="$1"
    shift
  fi
  # ---
  local KEY_VALUE="$1"
  local DELIMITER="$2" # '=', ':', ..
  local FIELD_VALUE="$3"
  # Check actuals:
  [[ $# -ge 3 && -n $KEY_VALUE && -n $DELIMITER ]] || {
    echo "Usage: user_config_set <KEY_VALUE> <DELIMITER> <FIELD_VALUE> [<FILE>].."
    return 2
    }
  # ---
  local INFILE OUTFILE
  local WORKINGFILE=$(mktemp)
  shift 3
  if [[ $# -eq 1 ]]; then
    INFILE="$1"
    OUTFILE="$1"
    { [[ -f "$INFILE" ]] && [[ -r "$INFILE" ]] && [[ -w "$INFILE" ]]; } || {
      echo "File $INFILE doesn't exist or doesn't have read-write permissions."
      echo "Usage: user_config_set <KEY_VALUE> <DELIMITER> <FIELD_VALUE> [<FILE>].."
      return 2
      }
  elif [[ $# -eq 0 ]]; then
    INFILE=/dev/stdin
    OUTFILE=/dev/stdout
  else
    local BOOLEAN_AND=1
    for INFILE in "$@"; do
      if user_config_set $IGNORE_UNCHANGED "$KEY_VALUE" "$DELIMITER" "$FIELD_VALUE" "$INFILE"; then
        BOOLEAN_AND=0;
      else
        true
      fi
    done
    return $BOOLEAN_AND
  fi
  cat "$INFILE" > $WORKINGFILE

  local FS="[ \t]*[$DELIMITER][ \t]*"
  local OFS="$DELIMITER"

  # Try to find the required binding:
  if \grep -q "^[ \t]*${KEY_VALUE}${FS}$(quoting_for_grep "${FIELD_VALUE}")[ \t]*$" $WORKINGFILE; then
    # Fine! no update required:
    rm $WORKINGFILE
    return $RETURN_CODE_WHEN_UNCHANGED
  fi

  # Unset keys at the same place if the delimiter appears twice:
  sed -i -e "s/^[ \t]*${KEY_VALUE}${FS}.*${FS}.*/${KEY_VALUE}${OFS}/" $WORKINGFILE

  # Try to update the file with `tabular_file_update':
  if tabular_file_update -d "$DELIMITER" --ignore-blanks -i -k 1 --key-value "$KEY_VALUE" -f 2 --field-value "$FIELD_VALUE" "$WORKINGFILE"; then
    cat $WORKINGFILE > $OUTFILE
    rm $WORKINGFILE
    return 0
  elif \grep -q "^[ \t]*${KEY_VALUE}${FS}$(quoting_for_grep "${FIELD_VALUE}")[ \t]*" $WORKINGFILE; then
    # Fine! no update performed:
    return $RETURN_CODE_WHEN_UNCHANGED
  else # No update occurred but it's needed, so:
    local TMPFILE=$(mktemp)
    # Remove comment any line "# <key><sep><value>":
    sed -e "s/^[#][#]*[ \t]*${KEY_VALUE}${FS}/${KEY_VALUE}${OFS}/" $WORKINGFILE > $TMPFILE
    if diff 1>/dev/null -q $TMPFILE "$WORKINGFILE"; then
      # There wasn't a commented line, so we append a new line:
      echo "${KEY_VALUE}${OFS}${FIELD_VALUE}" >> $WORKINGFILE
    else
      local CODE=0
      # There was a commented line, so we have just to update (if required) this line:
      tabular_file_update $IGNORE_UNCHANGED -d "$DELIMITER" --ignore-blanks -k 1 --key-value "$KEY_VALUE" -f 2 --field-value "$FIELD_VALUE" "$TMPFILE" > $WORKINGFILE || CODE=$?
      if [[ $CODE = 2 ]]; then return 2; fi
    fi
    cat $WORKINGFILE > $OUTFILE
    rm $TMPFILE $WORKINGFILE
    return 0
  fi
}


# =============================================================
#                   CONFIGURATION FILES
#             (not necessarily user-compliant)
#
#      Lines are supposed structured as: <KEY><SEP><VALUE>
#      where <SEP> is by default the regexp [ \t]*[=][ \t]*
# =============================================================

# Note that these files are considered not necessarily user-compliant
# in the sense that the update of a binding (key,value) it's not written
# at the line citing `key' in a comment or in a previous binding.
# Any previous binding with the same key is removed and the new binding is
# (re-)appended.
# For a more user-friendly update, call the function `user_config_set' defined
# below.

# Example of session:
# ---
# source toolkit_config_files.sh
# set_default_config_field_separator "="  # not really needed, the default would be suitable
# set_default_config_file "linux-3.2.44/.config"
# cp linux-3.2.44/.config{,.0}
# get_config_variable CONFIG_UML
# n
# set_config_variable CONFIG_UML y
# get_config_variable CONFIG_UML
# y
# set_config_variable CONFIG_UML '"YES"'
# get_config_variable CONFIG_UML
# "YES"
# get_config_variable_unquoting CONFIG_UML
# YES
# get_config_variable CONFIG_UML linux-3.2.44/.config.0
# n
# ---

# Global default used by configuration variable setter/getter:
function set_default_config_file {
 # global DEFAULT_CONFIG_FILE
 DEFAULT_CONFIG_FILE="$1"
}

# Set these defaults source-ing:
DEFAULT_CONFIG_FILE_FS="[ \t]*[=][ \t]*"
DEFAULT_CONFIG_FILE_OFS="="

# Examples:
# set_default_config_field_separator ":"
# set_default_config_field_separator "="
# set_default_config_field_separator "=" "="
# set_default_config_field_separator "[ \t]*[=][ \t]*" "="  # it's the default!
# set_default_config_field_separator "[ \t]*[:][ \t]*" "="
function set_default_config_field_separator {
 # global DEFAULT_CONFIG_FILE_{FS,OFS}
 if [[ $# = 2 ]]; then
   DEFAULT_CONFIG_FILE_FS="$1"
   DEFAULT_CONFIG_FILE_OFS="$2"
 elif [[ $# = 1 && $(echo -n "$1" | wc -c) = 1 ]]; then
   DEFAULT_CONFIG_FILE_FS="$1"
   DEFAULT_CONFIG_FILE_OFS="$1"
 else
   echo "Usage: set_default_config_field_separator <FS> [<OFS>]"
   return 2
 fi
}

# Set removing potential similar binding, then appending the
# provided binding.
# Note that the target ($3) is by default the file defined
# by the global variable DEFAULT_CONFIG_FILE
function set_config_variable {
 # global DEFAULT_CONFIG_FILE{,_FS,_OFS}
 local NAME=$1
 local VALUE="$2"
 local CONFIG_FILE=${3:-$DEFAULT_CONFIG_FILE}
 # ---
 local LOCAL_DEFAULT_FS="[ \t]*[=][ \t]*"
 local FS="${DEFAULT_CONFIG_FILE_FS:-$LOCAL_DEFAULT_FS}"
 local OFS=${DEFAULT_CONFIG_FILE_OFS:-=}
 sed -i -e "s/^${NAME}${FS}.*$//" $CONFIG_FILE
 echo "${NAME}${OFS}${VALUE}" >> $CONFIG_FILE
}

# Note that the target ($2) is by default the file defined
# by the global variable DEFAULT_CONFIG_FILE
function unset_config_variable {
 # global DEFAULT_CONFIG_FILE{,_FS}
 local NAME=$1
 local CONFIG_FILE=${2:-$DEFAULT_CONFIG_FILE}
 # ---
 local LOCAL_DEFAULT_FS="[ \t]*[=][ \t]*"
 local FS="${DEFAULT_CONFIG_FILE_FS:-$LOCAL_DEFAULT_FS}"
 local TMPFILE=$(mktemp)
 \grep -v "^${NAME}${FS}" $CONFIG_FILE >$TMPFILE
 cat $TMPFILE >$CONFIG_FILE
 rm -f $TMPFILE
}

function get_config_variable {
 # global DEFAULT_CONFIG_FILE{,_FS}
 local NAME=$1
 local CONFIG_FILE=${2:-$DEFAULT_CONFIG_FILE}
 # ---
 local LOCAL_DEFAULT_FS="[ \t]*[=][ \t]*"
 local FS="${DEFAULT_CONFIG_FILE_FS:-$LOCAL_DEFAULT_FS}"
 awk <$CONFIG_FILE -v NAME=$NAME -F "$FS" '$1 == NAME {print $2}'
}

function get_config_variable_unquoting {
 # global DEFAULT_CONFIG_FILE{,_FS}
 get_config_variable "$@" | sed -e 's/^"\(.*\)"$/\1/' -e "s/^'\(.*\)'$/\1/"
}

# Sort and merge configuration files removing comments and empty lines.
# We are supposing that the order of line is not important.
# TODO: it would be nice to implement `sort_and_merge_user_config_files'
function sort_and_merge_config_files {
 cat "$@" | awk 'NF>0 && $1 !~ /^#/' | sort | uniq
}

# Automatically export previously defined functions:
export -f $(awk '/^function/ {print $2}' ${BASH_SOURCE[0]})

