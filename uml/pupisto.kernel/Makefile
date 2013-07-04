# This file is part of Marionnet
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

help:
	@echo "Usage: make <KERNEL-VERSION>"
	@echo "   or: make OPTIONS=--custom <KERNEL-VERSION>"
	@echo "   or: make show_versions"
	@echo "   or: make dependencies"
	@echo "   or: make clean"
	@echo "   or: make help"
	@echo "Example: make 3.2.48"

2.% 3.%: dependencies
	./pupisto.kernel.sh $(OPTIONS) $@

clean:
	rm -rf _build.linux-*

# Show available (stable or longterm support) versions of the kernel
# looking to https://www.kernel.org/:
show_versions:
	@lynx 2>/dev/null --dump https://www.kernel.org/ | awk '/longterm:/ || /stable:/ {print $$1,$$2}'


# =============================================================
#                     Dependencies
# =============================================================

REQUIRED_PACKAGES=binutils wget patch ccache gcc gcc-multilib libc6-i386 libc6-dev-i386
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
