#!/bin/bash

set -e
SOURCE=${1:-"gui_world_gateway_dialog_widget.ml"}

# Requirements
ls -w1 $SOURCE gui_bricks.ml{,i}
type ocamlc
type lablgtk2

BASENAME=$(basename $0)
TMPDIR=$(mktemp -d /tmp/${BASENAME}_XXXXXXXX)
cp gui_bricks.ml{,i} $SOURCE $TMPDIR
#cp ../share/images/ico.world_gateway.dialog.png $TMPDIR

IMAGE_DIR="\"$PWD/../share/images/\""
echo cd $TMPDIR
cd $TMPDIR

ocamlc -c -I +lablgtk2 lablgtk.cma gui_bricks.mli
ocamlc -c -I +lablgtk2 lablgtk.cma gui_bricks.ml

# Manually resolving pointers ;-)
#sed -i -e "s/Initialization\.marionnet_home_images^//g" $SOURCE
ESCAPED_IMAGE_DIR=$(echo "$IMAGE_DIR" | sed -e 's/\//\\\//g')
SUBST='s/Initialization\.marionnet_home_images/'${ESCAPED_IMAGE_DIR}'/g'
sed -i -e $SUBST $SOURCE

echo "let _ = make ();;" >> $SOURCE
lablgtk2 -I . gui_bricks.cmo -init $SOURCE || true

rm -rf $TMPDIR

