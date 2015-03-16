#!/bin/bash

set -e
TOPLEVEL=${1:-utop}

which $TOPLEVEL &>/dev/null || {
  echo "Error: $0: $TOPLEVEL not found; install it please."
  exit 2
}

function realpath {
 local B=$(basename $1)
 local D=$(dirname $1)
 (builtin cd $D; echo $PWD/$B)
}

# Make
make compile_for_testing
make marionnet.cma

# Set the variable MARIONNET_HOME
source CONFIGME # set the variable `prefix'
MARIONNET_HOME=${prefix:-/usr/local}/share/marionnet

# Copy all modules to a single directory:
FLATTENED_DIRECTORY=_build/_build.flattened
mkdir -p $FLATTENED_DIRECTORY
find _build -path $FLATTENED_DIRECTORY -prune -o -type f -exec cp -fl {} $FLATTENED_DIRECTORY/ \;
find $MARIONNET_HOME -maxdepth 1 -type f -exec cp -fs {} $FLATTENED_DIRECTORY/ \;
pushd $FLATTENED_DIRECTORY; find $MARIONNET_HOME/* -maxdepth 0 -type d -exec ln -sf {} \;; popd

# Preamble:
PREAMBLE=$(mktemp)
cat > $PREAMBLE  <<EOF
#require "unix";;
#require "threads";;
#require "str";;
#require "lablgtk2";;
#require "lablgtk2.glade";;
#require "lablgtk2.sourceview2";;
#require "ocamlbricks";;
Ocamlbricks_log.enable ();;
Printexc.record_backtrace true;;
(* --- *)
#load "marionnet.cma";;
(* --- *)
Sys.chdir "$(realpath $FLATTENED_DIRECTORY)" ;;
let back = GMain.Main.main ;;
EOF

export OCAMLRUNPARAM=-b
# ---
cd $FLATTENED_DIRECTORY
ln -sf ../../etc
ln -sf ../../share
ln -sf ../../po

# Go:
utop -init $PREAMBLE || CODE=$?
rm -f $PREAMBLE
exit $CODE
