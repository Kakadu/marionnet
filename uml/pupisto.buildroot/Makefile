# This file is part of marionnet
# Copyright (C) 2013, 2014  Jean-Vincent Loddo
# Copyright (C) 2013, 2014  Université Paris 13
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

# File system creation based on Builroot (itself based on Busybox) and
# kernel patch (ghostification) and compilation

# This script builds from scratch a filesystem with the buildroot utilities.
# Be careful because sometimes buildroot needs some extras packages according
# to the choosen configuration.


# =============================================================
#                     Building `guignol'
# =============================================================

KERNEL_VERSION=3.2.60

guignol: dependencies buildroot
	./pupisto.buildroot.sh --kernel $(KERNEL_VERSION) --router --name guignol $(OPTIONS)

guignol-no-kernel: dependencies buildroot
	./pupisto.buildroot.sh --kernel $(KERNEL_VERSION) --router --no-kernel --name guignol $(OPTIONS)

guignol-custom: dependencies buildroot
	./pupisto.buildroot.sh --kernel $(KERNEL_VERSION) --router --name guignol --custom $(OPTIONS)

guignol-debug: dependencies buildroot
	./pupisto.buildroot.sh --debug --kernel $(KERNEL_VERSION) --router --name guignol $(OPTIONS)

# Make a minimal filesystem, just with Busybox and Bash (useful for testing), without quagga (no --router) and without kernel:
minimal:
	@{ test -f $(CUSTOM_PACKAGES_NO)  && cp -v $(CUSTOM_PACKAGES_NO)  $(CUSTOM_PACKAGES_NO).backup  --backup="numbered"; } || true
	@{ test -f $(CUSTOM_PACKAGES_YES) && cp -v $(CUSTOM_PACKAGES_YES) $(CUSTOM_PACKAGES_YES).backup --backup="numbered"; } || true
	@cat /dev/null > $(CUSTOM_PACKAGES_YES)
	@find ./_build.buildroot/package -name "Config.in" -exec \grep -o "BR2_PACKAGE_[A-Z0-9_]*" {} \; | uniq | sort | uniq > $(CUSTOM_PACKAGES_NO)
	./pupisto.buildroot.sh --kernel $(KERNEL_VERSION) --no-kernel --name minimal $(OPTIONS)


# =============================================================
#                     Dependencies
# =============================================================

buildroot: ./_build.buildroot

./_build.buildroot:
	git clone git://git.buildroot.net/buildroot
	mv buildroot ./_build.buildroot

REQUIRED_PACKAGES=whois texinfo git mercurial gcc-multilib unifdef ccache wget fakeroot patch
dependencies:
	@echo "Required packages: $(REQUIRED_PACKAGES)"
	@which dpkg 1>/dev/null || { echo "Not a Debian system (oh my god!); please install packages corresponding to: $(REQUIRED_PACKAGES)"; exit 1; }
	@dpkg 1>/dev/null -l $(REQUIRED_PACKAGES) || \
		if which aptitude; then \
		  sudo aptitude install -q -q -q -y $(REQUIRED_PACKAGES); \
		elif which apt-get; then \
		  sudo apt-get  install -q -q -q -y $(REQUIRED_PACKAGES); \
		else \
		  exit 1; \
		fi
	@echo Ok.


# =============================================================
#     Managing and quickly customizing Buildroot's packages
# =============================================================

# List all available Buildroot's packages
list-available: buildroot
	@find ./_build.buildroot/package -name Config.in -exec \grep -o "BR2_PACKAGE_[A-Z0-9_]*" {} \; \
	  | uniq | sort | uniq

# List Buildroot's packages cited by our building script:
LIST_CITED="\grep -o "BR2_PACKAGE_[A-Z0-9_][A-Z0-9_]*" ./pupisto.buildroot.sh | sort | uniq"
list-cited: buildroot
	@eval $(LIST_CITED)

TMPFILE1=/tmp/Makefile.pupisto.buildroot.1
TMPFILE2=/tmp/Makefile.pupisto.buildroot.2
TMPFILE3=/tmp/Makefile.pupisto.buildroot.3
TMPFILE4=/tmp/Makefile.pupisto.buildroot.4
TMPFILES=$(TMPFILE1) $(TMPFILE2) $(TMPFILE3) $(TMPFILE4)
CUSTOM_PACKAGES_NO=./_build.custom_packages_no
CUSTOM_PACKAGES_YES=./_build.custom_packages_yes

# List Buildroot's packages selected by our building script, plus packages manually
# selected with $(CUSTOM_PACKAGES_YES), minus package manually selected with $(CUSTOM_PACKAGES_NO).
# Note that if a package is customized both as "yes" and "no", the "yes" is prioritary.
# In other words, the list of selected packages is built using the formula:
# (cited DIFF no) UNION yes
list-selected: buildroot
	@eval $(LIST_CITED) | \grep -v "^BR2_PACKAGE_QUAGGA" > $(TMPFILE1)
	@cat /dev/null > $(TMPFILE2);
	@if test -f $(CUSTOM_PACKAGES_NO); then \grep -v "^#" $(CUSTOM_PACKAGES_NO)  | sort | uniq > $(TMPFILE2); fi
	@\grep <$(TMPFILE1) -v -w -F "$$(cat $(TMPFILE2))" > $(TMPFILE3)
	@cat /dev/null > $(TMPFILE4);
	@if test -f $(CUSTOM_PACKAGES_YES); then \grep -v "^#" $(CUSTOM_PACKAGES_YES) | sort | uniq > $(TMPFILE4); fi
	@cat $(TMPFILE3) $(TMPFILE4) | sort | uniq
	@rm -f $(TMPFILES)

# Quickly customize which packages you don't want in the filesystem:
customize-packages-no:  ./_build.custom_packages_no
	$$EDITOR $<

# Quickly customize which packages you want in the filesystem:
customize-packages-yes: ./_build.custom_packages_yes
	$$EDITOR $<

./_build.custom_packages_no: buildroot
	@test -f $@ || { make ./_build.custom_packages.initial && mv ./_build.custom_packages.initial $@; }

./_build.custom_packages_yes: buildroot
	@test -f $@ || { make ./_build.custom_packages.initial && mv ./_build.custom_packages.initial $@; }

./_build.custom_packages.initial:
	@find ./_build.buildroot/package -name Config.in -exec \grep -o "BR2_PACKAGE_[A-Z0-9_]*" {} \; | uniq | sort | uniq | awk '{print "#"$$0}' >$@


# =============================================================
#                      clean & help
# =============================================================

clean:
	rm -rf _build.*

help:
	@echo "Usage: make guignol"
	@echo "   or: make guignol-custom"
	@echo "   or: make minimal"
	@echo "   or: make customize-packages-yes"
	@echo "   or: make customize-packages-no"
	@echo "   or: make dependencies"
	@echo "   or: make clean"
	@echo "   or: make help-pupisto"
	@echo "   or: make help"
	@echo "Examples:"
	@echo "[1]  make guignol"
	@echo "[2]  make guignol-custom"
	@echo "[3]  make customize-packages-no customize-packages-yes guignol"
	@echo "[4]  make customize-packages-no customize-packages-yes guignol-custom"
	@echo "[5]  make help-pupisto"
	@echo "---"
	@echo "The example [2] gives you access to the Buildroot's menu configuration."
	@echo "The example [3] allows you to quickly customize the package selection removing comments from files \`_build.custom_packages_{yes,no}'."
	@echo "The example [4] provides you both customization methods."
	@echo "The example [5] is simply equivalent to \`pupisto.buildroot.sh --help'; you can launch directly this script if you want to create the Buildroot's filesystem with special options (name, config)."


help-pupisto:
	./pupisto.buildroot.sh --help
