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
	@echo "   or: make show_versions"
	@echo "   or: make dependencies"
	@echo "   or: make clean"
	@echo "   or: make help"
	@echo "Example: make 3.2.44"

2.% 3.%: dependencies
	./pupisto_kernel.sh $@

clean:
	rm -rf _build.linux-*

dependencies:
	if which aptitude; then sudo aptitude install -q -q -q -y binutils wget gcc patch ccache; fi

# Show available (stable or longterm support) versions of the kernel
# looking to https://www.kernel.org/:
show_versions:
	@lynx 2>/dev/null --dump https://www.kernel.org/ | awk '/longterm:/ || /stable:/ {print $$1,$$2}'

