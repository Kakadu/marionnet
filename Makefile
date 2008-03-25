# This -*- makefile -*- is part of our build system for OCaml projects
# Copyright (C) 2008  Luca Saiu
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

# This is the revision of 2008-03-25.


######################################################################
# This make file is
# general-purpose: the actual project-dependant part should be
# written for each specific project in a 'Makefile.local' file.
#
# This contains some (simple) makefile magic which is only supported
# by GNU Make. Don't even try to use other make implementations.
######################################################################


######################################################################
# Implementation of targets. Note that the user is *not* supposed to
# override these, but only to define the project-dependant '-local'
# versions:

# The main target. Its implementation is entirely project-dependant:
main: _tags myocamlbuild.ml main-local
	@(for x in $(PROGRAMS) $(LIBRARIES); do \
	  echo "Building \"$$x\"..."; \
	  $(MAKE) $$x; \
	  echo "Ok, \"$$x\" was built with success."; \
	done && \
	echo "Success.")

# 'all' is just an alias for 'main':
all: main

# In some projects we may need to build something more than 'main',
# but we do nothing more by default:
world: world-local main
	@(echo 'Success.')

# Generate ocamldoc documentation:
ocamldoc: main ocamldoc-local
	@(ocamlbuild ocamlbricks.docdir/index.html &> /dev/null && \
	rm ocamlbricks.docdir && \
	echo 'Success.'; \
	echo 'The documentation has been built with success under _build/')

# Install programs and libraries:
install: install-programs install-libraries install-data install-local
	@(echo 'Success.')


# The user is free to override this to add custom targets to install into the
# $prefix/share/$name installation directory:
OTHER_DATA_TO_INSTALL =

# Install the data from this package into $prefix/share/$name:
install-data: main install-data-local
	@($(call READ_CONFIG, prefix); \
	$(call READ_META, name); \
	directory=$$prefix/share/$$name; \
	shopt -s nullglob; \
	if [ -e share ]; then \
	  dataifany=`ls -d share/*`; \
	else \
	  dataifany=''; \
	fi; \
	if [ "$$dataifany" == "" ]; then \
	  echo "No data to install: ok, no problem..."; \
	else \
	  echo "Installing $$name data into $$directory ..."; \
	  echo "Creating $$directory ..."; \
	  if mkdir -p $$directory; then \
	    echo "The directory $$directory was created with success."; \
	  else \
	    echo "Could not create $$directory"; \
	    exit -1; \
	  fi; \
	  echo "Copying $$name data to $$directory ..."; \
	  for x in COPYING README $$dataifany $(OTHER_DATA_TO_INSTALL); do \
	    if cp -af $$x $$directory; then \
	      echo "Installed $$x into $$directory/"; \
	    else \
	      echo "Could not write $$directory/$$x."; \
	      exit -1; \
	    fi; \
	  done; \
	  echo "Data installation for $$name was successful."; \
	fi)

# Remove the data of this package from $prefix/share/$name:
uninstall-data: uninstall-data-local
	@(($(call READ_CONFIG, prefix); \
	$(call READ_META, name); \
	directory=$$prefix/share/$$name; \
	echo "Removing $$name data from $$prefix/share/..."; \
	shopt -s nullglob; \
	if rm -rf $$directory; then \
	  echo "The entire directory $$directory was removed."; \
	else \
	  echo "Could not delete $$directory"; \
	  exit -1; \
	fi); \
	echo 'Data uninstallation was successful.')

# The user is free to override this to add custom targets to install into the
# $prefix/bin installation directory:
OTHER_PROGRAMS_TO_INSTALL =

# Install the programs from this package into $prefix/bin:
install-programs: main install-programs-local
	@($(call READ_CONFIG, prefix); 		     \
	$(call READ_META, name);   		     \
	echo "Creating $$prefix/bin/..."; \
	(mkdir -p $$prefix/bin &> /dev/null || true); \
	echo "Installing programs from $$name into $$prefix/bin/..."; \
	shopt -s nullglob; \
	for file in $(OTHER_PROGRAMS_TO_INSTALL) _build/*.byte _build/*.native; do \
	    echo "Installing "`basename $$file`" into $$prefix/bin..."; \
	    cp -a $$file $$prefix/bin; \
	done) && \
	echo 'Program installation was successful.'

# Remove the programs from this package from $prefix/bin:
uninstall-programs: main uninstall-programs-local
	@($(call READ_CONFIG, prefix); 		     \
	$(call READ_META, name);   		     \
	echo "Removing $$name programs from $$prefix/bin/..."; \
	shopt -s nullglob; \
	for file in $(OTHER_PROGRAMS_TO_INSTALL) _build/*.byte _build/*.native; do \
	    export pathname=$$prefix/bin/`basename $$file`; \
	    echo "Removing $$pathname..."; \
	    rm -f $$pathname; \
	done) && \
	echo 'Program uninstallation was successful.'

# The user is free to override this to add custom targets to install into the
# library installation directory:
OTHER_LIBRARY_FILES_TO_INSTALL =

# Install the library in this package into the path chosen at configuration time:
install-libraries: main install-libraries-local
	@($(call READ_CONFIG,libraryprefix); 		     \
	$(call READ_META,name);        			     \
	if [ "$(LIBRARIES)" == "" ]; then \
	  echo "There are no libraries to install: ok, no problem..."; \
	else \
	  (echo "Installing $$name libraries into $$libraryprefix/$$name/..."; \
	  (mkdir $$libraryprefix/$$name &> /dev/null || true); \
	  shopt -s nullglob; \
	  cp -f META $(OTHER_LIBRARY_FILES_TO_INSTALL) \
	        _build/*.cma _build/*.cmxa _build/*.a \
	        `find _build/ -name \*.cmi | grep -v /myocamlbuild` \
	        `find _build/ -name \*.mli | grep -v /myocamlbuild` \
	      $$libraryprefix/$$name/) && \
	  echo 'Library installation was successful.'; \
	fi)

# Uninstall programs and libraries:
uninstall: uninstall-programs uninstall-libraries uninstall-data uninstall-local
	@(echo 'Success.')

# Remove the library from the installation path chosen at configuration time:
uninstall-libraries: main uninstall-libraries-local
	@(($(call READ_CONFIG,libraryprefix); 		     \
	$(call READ_META,name);        			     \
	echo "Uninstalling $$name libraries from $$libraryprefix/..."; \
	shopt -s nullglob; \
	rm -rf $$libraryprefix/$$name/) && \
	echo 'Library uninstallation was successful.')

# Make a source tarball:
dist: clean dist-local
	@($(call READ_META, name, version); \
	$(call FIX_VERSION); \
	echo "Making the source tarball _build/$$name-$$version.tar.gz ..."; \
	mkdir -p _build/$$name-$$version; \
	cp -af * _build/$$name-$$version/ &> /dev/null; \
	(tar --exclude=_build --exclude=_darcs -C _build -czf \
	     _build/$$name-$$version.tar.gz $$name-$$version/ && \
	rm -rf _build/$$name-$$version)) && \
	echo "Success."

# These files are included also in binary tarballs:
FILES_TO_ALWAYS_DISTRIBUTE = \
  COPYING README INSTALL AUTHORS THANKS META Makefile Makefile.local CONFIGME

# Make a binary tarball:
dist-binary: dist-binary-local main #ocamldoc
	@($(call READ_META, name, version); \
	$(call FIX_VERSION); \
	architecture=$$(echo `uname -o`-`uname -m` | sed 's/\//-/g'); \
	directoryname=$$name-$$version--binary-only--$$architecture; \
	filename=$$directoryname.tar.gz; \
	echo "Making the binary tarball _build/$$filename ..."; \
	mkdir -p _build/$$directoryname; \
	mkdir -p _build/$$directoryname/_build; \
	shopt -s nullglob; \
	for x in $(FILES_TO_ALWAYS_DISTRIBUTE); do \
	  cp $$x _build/$$directoryname; \
	done; \
	for x in $(PROGRAMS) $(LIBRARIES); do \
	  cp _build/$$x _build/$$directoryname/_build; \
	done; \
	for x in `find _build/ -name \*.cmi | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.mli | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.cma | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.cmxa | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.a | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.byte | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	         `find _build/ -name \*.native | grep -v /myocamlbuild | grep -v _build/$$directoryname` \
	; do \
	  cp $$x _build/$$directoryname/_build; \
	done; \
	for x in _build/*.docdir; do \
	  cp -af $$x _build/$$directoryname; \
	done; \
	for x in main main-local install-libraries-local install-programs-local \
	         install-local install-data-local clean clean-local \
		 _tags myocamlbuild.ml \
	; do \
		echo "This dummy file prevents make from building the \"$$x\" target." \
		  > _build/$$directoryname/$$x; \
	done; \
	(tar czf _build/$$filename -C _build $$directoryname/ && \
	rm -rf _build/$$directoryname)) && \
	echo "Success."

# Remove generated stuff:
clean: clean-local
	@(rm -rf _build; \
	find -type f -name \*~ -exec rm -f {} \;; \
	find -type f -name \#\*\# -exec rm -f {} \;; \
	find -type f -name core -exec rm -f {} \;; \
	rm -f _tags myocamlbuild.ml)

# Meta-help about the targets defined in this make file:
targets:
	@cat Makefile Makefile.local | grep -B 1 "^[a-z0-9_-]*[:]" | \
	  awk '/BEGIN/ {r=""} /^[#]/ { r=substr($$0,2); next; } /^[a-z0-9_-]*[-]local[:]/ {r=""; next} /^[a-z0-9_-]*[:]/{split($$0,a,/:/); printf("%s\r\t\t\t--- %s\n",a[1],r); r=""; next} {r=""}' | sort


######################################################################
# Default implementation for '-local' targets:

# All the user-definable '-local' targets have an empty implementation
# by default:
main-local:
world-local:
ocamldoc-local:
install-local:
uninstall-local:
install-programs-local:
uninstall-programs-local:
install-libraries-local:
uninstall-libraries-local:
install-data-local:
uninstall-data-local:
dist-local:
dist-binary-local:
clean-local:

# Let's avoid confusion between all and main: they're the same thing
# for us, and we only support main-local:
all-local:
	echo 'all-local does not exist. Use main-local instead'
	exit 1


#####################################################################
# Default compilation flags. The user *is* expected to override or
# extend these:
PROGRAMS =
LIBRARIES =
COMPILE_OPTIONS = -thread
DIRECTORIES_TO_INCLUDE =
LIBRARIES_TO_LINK =


#####################################################################
# Default rules:

# Bytecode libraries:
%.cma: _tags myocamlbuild.ml
	@(ocamlbuild $@)
# Native libraries:
%.cmxa: _tags myocamlbuild.ml
	@(ocamlbuild $@)
# Bytecode programs:
%.byte: _tags myocamlbuild.ml
	@(ocamlbuild $@; \
	rm $@)
# Native programs:
%.native: _tags myocamlbuild.ml
	@(ocamlbuild $@; \
	rm $@)


#####################################################################
# Some macros, used internally and possibly by Makefile.local:

# Macro extracting, via source, the value associated to some keys
# $(2),..,$(9) in a file $(1).
#
# Example: 
#	$(call SOURCE_AND_TEST,CONFIGME,prefix);
#	$(call SOURCE_AND_TEST,CONFIGME,prefix,libraryprefix);
SOURCE_AND_TEST = \
	if ! source $(1) &> /dev/null; then        		\
		echo 'Evaluating $(1) failed.';    		\
		exit 1;                           		\
	fi;                                        		\
	for i in $(2) $(3) $(4) $(5) $(6) $(7) $(8) $(9); do 	\
		CMD="VAL=$$`echo $$i`"; eval $$CMD;		\
	 	if test -z "$$VAL"; then                  	\
			echo "FATAL: $${i} is undefined in $(1)."; 	\
			exit 1;                            		\
		fi; 						\
	done;							\
	unset CMD VAL i


# Macro extracting, via grep, the value associated to keys
# $(2),..,$(9) in a file $(1).
#
# Examples: 
#	$(call GREP_AND_TEST,META,name); 
#	$(call GREP_AND_TEST,META,name,version); 
GREP_AND_TEST = \
	for i in $(2) $(3) $(4) $(5) $(6) $(7) $(8) $(9); do 	\
		if ! CMD=`grep "^$$i=" $(1)`; then                 	\
			echo "FATAL: $$i is undefined in $(1).";	\
			exit 1;                            		\
		fi; 							\
		eval $$CMD;						\
	done;							\
	unset CMD i

# Instance of SOURCE_AND_TEST: source the file "CONFIGME" and test 
# if the given names are defined
#
# Example:
# 	$(call READ_CONFIG,prefix,libraryprefix);
#
READ_CONFIG = \
	$(call SOURCE_AND_TEST,CONFIGME,$(1),$(2),$(3),$(4),$(5),$(6),$(7),$(8),$(9))

# Instance of GREP_AND_TEST: read the file "META" searching for a names
# for all given names
#
# Example:
#	$(call READ_META,name,version); 		
#
READ_META = \
	$(call GREP_AND_TEST,META,$(1),$(2),$(3),$(4),$(5),$(6),$(7),$(8),$(9))

# If the value of the 'version' variable contains the substring 'snapshot' then
# append to its value the current date, in hacker format. 'version' must be already
# defined. No arguments, no output.
FIX_VERSION = \
	if echo $$version | grep snapshot &> /dev/null; then \
	  version="$$version-"`date +"%Y-%m-%d"`; \
	fi


# A simple macro automatically finding all the subdirectories containing ML sources,
# setting the variable 'sourcedirectories' to a string containing all such
# subdirectories, alphabetically sorted, separated by spaces, and finally echo'ing
# the value of sourcedirectories:
SOURCE_SUBDIRECTORIES = \
	sourcedirectories=''; \
	for d in `find -type d | grep -v /_darcs\$$ | grep -v /_darcs/ \
	          | grep -v /_build\$$ | grep -v /_build/ \
	          | grep -v ^.$$ | sort`; do \
		if ls $$d/*.ml &> /dev/null  || \
	           ls $$d/*.mli &> /dev/null || \
		   ls $$d/*.mll &> /dev/null || \
		   ls $$d/*.mly &> /dev/null ; then \
			sourcedirectories+="$$d "; \
		fi; \
	done; \
	echo $$sourcedirectories

# Set the shell variable $(1) as the string obtained by prefixing each token
# in $(2) with the prefix $(3): for example if the shell variable
# 'sourcedirectories' is set to './A ./B' then
#     $(call ADD_PREFIX_TO_EACH_WORD, includes, $$sourcedirectories, -I)
# sets the shell variable 'includes' to '-I ./A -I ./B '.
# The value of $(1) is finally echo'ed.
ADD_PREFIX_TO_EACH_WORD = \
	$(call SOURCE_SUBDIRECTORIES); \
	result=''; \
	for element in $(2); do \
		result+="$(3) $$element "; \
	done; \
	$(1)=$$result; \
	echo $$result

# This macro expands to the project name, extracted from META. No parameters.
# Example:
#   echo "$(call PROJECT_NAME) is beautiful."
PROJECT_NAME = \
	$$( $(call GREP_AND_TEST,META,name); \
	echo $$name )

# We automatically generate the _tags file needed by OCamlBuild.
# Every subdirectory containing sources is included. This may be more than what's needed,
# but it will always work and require no per-project customization. sed is used to remove
# the initial './' from each directory. We refer some settings implemented in our (still
# automatically generated) OCamlBuild plugin.
_tags:
	@(echo -e "# This file is automatically generated. Please don't edit it.\n" > $@; \
	for directory in $$( $(call SOURCE_SUBDIRECTORIES) ); do \
		directory=`echo $$directory | sed s/^.\\\\///`; \
		echo "<$$directory>: include" >> $@; \
	done; \
	echo >> $@; \
	echo "<**/*.{ml,byte,native,cma}>: ourincludesettings" >> $@; \
	echo "<**/*.cmxa>: ourincludesettings" >> $@; \
	echo "<**/*.byte>: ourincludesettings, ourbytelinksettings" >> $@; \
	echo "<**/*.native>: ourincludesettings, ournativelinksettings" >> $@; \
	echo "<**/*.ml>: ourocamldocsettings" >> $@)

# We automatically generate the OCamlBuild plugin customizing the build process
# with our user-specified options, include directories, etc.:
myocamlbuild.ml:
	@(echo -e "(* This file is automatically generated. Please don't edit it. *)\n" > $@; \
	echo -e "open Ocamlbuild_plugin;;\n" >> $@; \
	echo -en "let our_compile_options = [ " >> $@; \
	for x in $(COMPILE_OPTIONS); do \
		echo -en "A \"$$x\"; " >> $@; \
	done; \
	echo -e "];;" >> $@; \
	echo -en "let our_include_options = [ " >> $@; \
	for x in $(DIRECTORIES_TO_INCLUDE); do \
		echo -en "A \"-I\"; A \"+$$x\"; " >> $@; \
	done; \
	echo -e "];;" >> $@; \
	echo -en "let our_byte_link_options = our_include_options @ [ " >> $@; \
	for x in $(LIBRARIES_TO_LINK); do \
		echo -en "A \"$$x.cma\"; " >> $@; \
	done; \
	for x in $(OBJECTS_TO_LINK); do \
		echo -en "A \"$$x.cmo\"; " >> $@; \
	done; \
	echo -e "];;" >> $@; \
	echo -en "let our_native_link_options = our_include_options @ [ " >> $@; \
	for x in $(LIBRARIES_TO_LINK); do \
		echo -en "A \"$$x.cmxa\"; " >> $@; \
	done; \
	for x in $(OBJECTS_TO_LINK); do \
		echo -en "A \"$$x.cmx\"; " >> $@; \
	done; \
	echo -e "];;\n" >> $@; \
	echo -e "dispatch (function After_rules ->" >> $@; \
	echo -e "  flag [\"ocaml\"; \"compile\"; \"ourincludesettings\"]" >> $@; \
	echo -e "       (S (our_compile_options @ our_include_options));" >> $@; \
	echo -e "  flag [\"ocaml\"; \"link\"; \"ourbytelinksettings\"]" >> $@; \
	echo -e "       (S (our_compile_options @ our_byte_link_options));" >> $@; \
	echo -e "  flag [\"ocaml\"; \"link\"; \"ournativelinksettings\"]" >> $@; \
	echo -e "       (S (our_compile_options @ our_native_link_options));" >> $@; \
	echo -e "  flag [\"ocaml\"; \"doc\"; \"ourocamldocsettings\"]" >> $@; \
	echo -e "       (S ([A \"-keep-code\"; A \"-colorize-code\"] @ our_include_options));" >> $@; \
	echo -e "  | _ -> ());;" >> $@)

###########################################################################
# Include the project-dependant file (if any) which implements the '-local'
# targets:
-include Makefile.local
