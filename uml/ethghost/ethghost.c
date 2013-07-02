/*
 * This file is a part of the tool ethghost to the
 * Marionnet project <http://www.marionnet.org>
 *
 * Copyright (C) 2009  Jonathan Roudiere
 * Licence GPLv2+ : GNU GPL version 2 or later;
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * This is the revision of 2009-07-07.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
/* need to IFNAMSIZ */
#include <net/if.h>
/* Ghostification interface */
#include "ethghost-interface.h"

static int usage(void)
{
	fprintf(stderr, "ethghost (%s) usage :\n\n", __ETHGHOST_VERSION__);
	fprintf(stderr, "ethghost -h, --help                   this help message\n");
	fprintf(stderr, "         -v, --version                get the version\n");
	fprintf(stderr, "         -g, --ghostify DEVICE        ghostify this interface\n");
	fprintf(stderr, "         -u, --unghostify DEVICE      unghostify this interface\n\n");
	fprintf(stderr, "DEVICE : is the name of a network interface (like : eth0, lo)\n");
	return (EXIT_SUCCESS);
}

static int version(void)
{
	printf("\nethghost " __ETHGHOST_VERSION__ " \n");
	printf("Copyright (C) 2009  Jonathan Roudiere\n");
	printf("Copyright (C) 2009, 2013 Université Paris 13\n");
	printf("License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n");
	printf("\nThis is free software: you are free to change and redistribute it.\n");
	printf("There is NO WARRANTY, to the extent permitted by law.\n\n");
	return (EXIT_SUCCESS);
}


int main (int argc, char *argv[])
{
	int error = 0;
	char *prog;
	unsigned int (*act)(const char *iface);

	/* Get the binary name */
	prog = basename(argv[0]);

	/* debug */
	dprintf("Start of %s ",argv[0]);

	/* Verify number of args, if need show version/help */
	if (argc == 1) {
		dinfo;
		fprintf(stderr, "%s: Error, no args. Exit!!\n\n",prog);
		usage();
		return (EXIT_FAILURE);
	} else {
		dinfo;
		/* the one and only case where we accept one option */
		if (argc == 2) {
			/* Look if version is asked */
			dprintf("argc = %i and argv[%i] = %s, look if (-v,--version) has been provided.", argc, argc, argv[argc - 1]);
			if ((!(strcmp(argv[1],"-v"))) || (!(strcmp(argv[1],"--version")))) {
				version();
				return (EXIT_SUCCESS);
			} else {
				/* Look if help is asked */
				dprintf("argc = %i and argv[%i] = %s, look if (-h,--help) has been provided.", argc, argc, argv[argc - 1]);
				if ((!(strcmp(argv[1],"-h"))) || (!(strcmp(argv[1],"--help")))) {
					usage();
					return (EXIT_SUCCESS);
				} else {
					fprintf(stderr, "%s: Error, unknown option. Exit!!\n\n",prog);
					usage();
					return (EXIT_FAILURE);
				}
			}
		}
		if (argc != 3) {
			fprintf(stderr, "%s: Error, bad number of arguments. Exit!!\n\n",prog);
			usage();
			return (EXIT_FAILURE);
		}
	}

	/* Search options used */

	/* debug */
	dprintf("argc = %i, argv[1] = %s and argv[2] = %s, search options used.", argc, argv[1], argv[2]);

	/* put in act pointer toward appropriate function */
	if ((!(strcmp(argv[1],"-g"))) || (!(strcmp(argv[1],"--ghostify")))) {
		dprintf("Function call to act : act = &(ghostify_iface);");
		act = &(ghostify_iface);
	} else {
		if ((!(strcmp(argv[1],"-u"))) || (!(strcmp(argv[1],"--unghostify")))) {
			dprintf("Function call to act : act = &(unghostify_iface);");
			act = &(unghostify_iface);
		} else {
			fprintf(stderr, "%s: Error, unknown option. Exit!!\n\n",prog);
			usage();
			return (EXIT_FAILURE);
		}
	}

	/* Verify lenght of the second args */
	if (strlen(argv[2]) >= IFNAMSIZ) {
		fprintf(stderr, "%s: Error, invalid interface name. Exit!!\n",prog);
		return (EXIT_FAILURE);
	}

	/* Act */
	if ((error = act(argv[2])) == 0) {
		dprintf("Act exit without error (%i)", error);
		if (act == (&ghostify_iface)) {
			printf("ethghost: SUCCESS, the interface %s has been ghostified!!\n", argv[2]);
		} else {
			if (act == (&unghostify_iface)) {
				printf("ethghost: SUCCESS, the interface %s has been unghostified!!\n", argv[2]);
			} else {
				/* debug, never come here */
				fprintf(stderr, "\nethghost: Error, an unexpected error (bug?) took place. Exit!!\n");
				return (EBUG);
			}
		}
		return (EXIT_SUCCESS);
	} else {
		dprintf("Act exit with error (%i)", error);
		/* explicit exit message have already been done */
		return (error); /* report real error code to the user */
	}

	/* Not necessary - BUG */
	return (EBUG);
}
