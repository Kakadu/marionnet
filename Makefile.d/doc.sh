#!/bin/bash -e

# This -*- makefile -*- is part of our build system for OCaml projects
# Copyright (C) 2008  Jean-Vincent Loddo

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

# Usage:
# doc.sh -pp "$(PP_OPTION)" -e "$(UNDOCUMENTED)" -i $(DIRECTORIES_TO_INCLUDE)

################################
#   Set ocamldoc parameters    #
################################

UNDOCUMENTED="meta.ml myocamlbuild.ml"

function usage {
 echo 'Usage (in a Makefile):'
 echo 'doc.sh -pp "$(PP_OPTION)" -e "$(UNDOCUMENTED)" -i $(DIRECTORIES_TO_INCLUDE)'
 exit 1
}
set -x

# The first argument may be empty but must be present.
[[ $1 = "-pp" ]] || usage
PP_OPTION=$(echo $2)
if [[ $PP_OPTION != "" ]]; then
 PP_OPTION="-pp '$2'"
fi
shift 2

[[ $1 = "-e" ]] || usage
UNDOCUMENTED+=" "$(echo $2)
for i in $UNDOCUMENTED; do
  UNDOCUMENTED_FILTER+=" -a ! -name $i"
done
shift 2

[[ $1 = "-i" ]] || usage
shift
for i in "$@"; do
 INCLUDE_LIBS+=" -I +$i"
done

# ocamldoc parameters:

SOURCES=$(builtin cd _build/ && find . \( -name "*.ml" -o -name "*.mli" \) $UNDOCUMENTED_FILTER)
INCLUDES=$(builtin cd _build/ && find . -type d -printf "-I %p\n")
PROJECT=$(basename $PWD)

################################
#    Make header and footer    #
################################

cd _build/
mkdir -p doc/html

# Make header.gif
function enrich_index_html {

 which dot || {
   echo "Warning: you need dot (graphviz) in order to generated the documentation header.";
   return 0
   }

 # Get user-defined header and footer
 [[ -f ../header.html ]] && HEADER_FILE=$(< ../header.html)
 [[ -f ../footer.html ]] && FOOTER_FILE=$(< ../footer.html)
 [[ -f ../AUTHORS     ]] && AUTHORS_FILE=$(< ../AUTHORS)
 [[ -f ../AUTHORS     ]] && AUTHORS_FILE=$(awk <../AUTHORS '/$/ {print; print "<br/>"; }')

 # Make dependencies graph
 set -x;
 eval ocamldoc $PP_OPTION -dot -d doc/html/ -o doc/html/header0.dot -colorize-code $INCLUDES $INCLUDE_LIBS $SOURCES
 set +x
 echo 'Ok, the dependencies graph was built with success.'
 pushd doc/html >/dev/null
 grep -v "rankdir=\|size=\|rotate=" header0.dot > header.dot
 dot header.dot -Tgif -o header.gif
 HEADER=$(cat <<EOF
<center>
Project
<h1>$PROJECT</h1>
<br/>
<hr/>
<a href="#dependencies">Dependencies</a>
<a href="#authors">License and authors</a>
</center>
$HEADER_FILE
<hr/>
EOF
)

 FOOTER=$(cat <<EOF
<br/>
<hr/>
<center>
<h1>Dependencies</h1>
<br/>
<p/><img id="dependencies" src="header.gif" align="middle" alt="Dependencies">
</center>
$FOOTER_FILE
<hr/>
<h1 id="authors">License and authors</h1>
<br/>
<p>
$AUTHORS_FILE
</p>
EOF
)

 cat index.html | awk -v h="$HEADER" -v f="$FOOTER" '
    /^<center><h1>.*<.h1><.center>/ {print h; next}
    /^<\/body>/                     {print f "</body>"; next}
    {print}
    ' > index1.html
 mv -f index1.html index.html
 popd >/dev/null
 echo 'Ok, header and footer have been included into index.html.'
}


################################
#        Call ocamldoc         #
################################

set -x
eval ocamldoc -t $PROJECT $PP_OPTION -sort -m A -keep-code -html -colorize-code -d doc/html/ $INCLUDES $INCLUDE_LIBS $SOURCES
set +x
echo 'Ok, the documentation was built with success.'
enrich_index_html
echo 'The documentation has been built with success under _build/doc/html'
