# This file is a part of the tool ethghost to the
# Marionnet project <http://www.marionnet.org>

# Copyright (C) 2011  Luca Saiu
# Licence GPLv2+ : GNU GPL version 2 or later;

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Shall we use Autotools here?  The thing is really small, and
# probably inherently non-portable: it only makes sense on GNU/Linux
# with our kernel patch...  So I don't think using the Autotools is
# worth the hassle here.  For the time being I've written this trivial
# Makefile.  -- Luca Saiu, October 2011


CC = gcc
CFLAGS = -g -O2

all: ethghost

ethghost: ethghost.o ethghost-interface.o
	$(CC) $(CFLAGS) -o $@ $^

ethghost.o: ethghost.c ethghost-interface.h
	$(CC) $(CFLAGS) -c -o $@ $<

ethghost-interface.o: ethghost-interface.c ethghost-interface.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f *.o ethghost *~

# DESTDIR may be set by the caller, for instance:
# make DESTDIR=/usr/local install
# (suitable for buildroot)
install:
	 cp ethghost $(DESTDIR)/bin/ethghost

# Used by `pupisto' to know the package version to include in the buildroot's image
print_version:
	@awk <ethghost-interface.h '($$1 == "#define") && ($$2 == "__ETHGHOST_VERSION__") {a=$$3; gsub(/"/,"",a); print a}'
