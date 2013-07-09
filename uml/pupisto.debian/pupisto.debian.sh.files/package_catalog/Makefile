# This file is part of Marionnet, a virtual network laboratory
# Copyright (C) 2013  Jean vincent Loddo
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

# This Makefile helps people to build a debian filesystem
# with debootstrap according to the Marionnet requirements.

# ----------------------
#       binary_list
# ----------------------

binary_list.UNION:
	cat binary_list.*.* | sort -d | uniq > $@

# ----------------------
#   wheezy or squeeze
# ----------------------

wheezy squeeze: binary_list.UNION
	chmod +x ./make_package_catalog_from_binary_list.sh
	./make_package_catalog_from_binary_list.sh $@

# ----------------------
#       wheezy
# ----------------------

package_catalog.wheezy.GENERATED: wheezy

package_catalog.wheezy.COMPLETE.COMMENTED: package_catalog.wheezy.GENERATED
	touch package_catalog.wheezy.additional
	cat $< package_catalog.wheezy.additional | sort -k 1,1 -d | awk '{print "#"$$0}' > $@

WHEEZY_SELECTION=package_catalog.wheezy.selection

package_catalog.wheezy.COMPLETE.COMMENTED.selection: package_catalog.wheezy.COMPLETE.COMMENTED
	cp -f $< $@;
	if [[ -f $(WHEEZY_SELECTION) ]]; then for i in $$(\grep -v '^#' $(WHEEZY_SELECTION) | awk '{print $$1}'); do sed -i -e "s/^#$${i} /$${i} /" $@; done; else cp -f $< $(WHEEZY_SELECTION); fi


# ----------------------
#       squeeze
# ----------------------

package_catalog.squeeze.GENERATED: squeeze

package_catalog.squeeze.COMPLETE.COMMENTED: package_catalog.squeeze.GENERATED
	touch package_catalog.squeeze.additional
	cat $< package_catalog.squeeze.additional | sort -k 1,1 -d | awk '{print "#"$$0}' > $@

SQUEEZE_SELECTION=package_catalog.squeeze.selection

package_catalog.squeeze.COMPLETE.COMMENTED.selection: package_catalog.squeeze.COMPLETE.COMMENTED
	cp -f $< $@;
	if [[ -f $(SQUEEZE_SELECTION) ]]; then for i in $$(\grep -v '^#' $(SQUEEZE_SELECTION) | awk '{print $$1}'); do sed -i -e "s/^#$${i} /$${i} /" $@; done; else cp -f $< $(SQUEEZE_SELECTION); fi

# ----------------------
#     Dependencies
# ----------------------

REQUIRED_PACKAGES=debootstrap coreutils
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

# ----------------------
#       clean
# ----------------------

clean:
	sudo rm -rf _build.*;
	rm -f *~
	rm -f package_catalog.*.COMPLETE.COMMENTED  package_catalog.*.COMPLETE.COMMENTED.selection
	@if test -f package_catalog.wheezy.GENERATED; then echo "Warning: file \`package_catalog.wheezy.GENERATED' not removed; please remove it manually if you really know what are you doing."; fi
	@if test -f package_catalog.squeeze.GENERATED; then echo "Warning: file \`package_catalog.squeeze.GENERATED' not removed; please remove it manually if you really know what are you doing."; fi

