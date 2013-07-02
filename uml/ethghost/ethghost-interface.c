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
 * Minor changes by Jean-Vincent Loddo 2013/04/12 (ghost2 -> ethghost)
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <errno.h>
#include <libgen.h>
#include "ethghost-interface.h"

/*
 * functions used internally by __{create,destroy}_socket and
 * __{un,}ghostify to display and explained errors.
 * origcode - variable use by the caller to identify itself
 * nerror 	- errno provide by the caller
 * return	- nerror
 */
static int on_error (int origcode, int nerror)
{
	switch (origcode)
	{
		/*
		 * If we can not create socket then we send a generic message
		 * with the original system error message between parentheses.
		 */
		case (ESOCKCREATE) :
			dinfo;
			fprintf(stderr, "ethghost: Error: couldn't create socket (%s)\n",strerror(nerror));
			break;
		/*
		 * If we can not destroy socket then we send a generic message
		 * with the original system error message between parentheses.
		 */
		case (ESOCKDELETE) :
			dinfo;
			fprintf(stderr, "ethghost: Error: couldn't destroy socket (%s)\n",strerror(nerror));
			break;
		/*
		 * If errors occur during the ghostification or the unghostification
		 * operation, we be able to provide (in general) a more explicit message
		 * than the system (which don't know ghost operations).
		 */
		case (EGHOSTIFY) :
			dinfo;
			switch (nerror)
			{
				/*
				 * This error code is send by the ghostification kernel code if
				 * the lenght of the iface that we try to ghositify is null or
				 * greater than IFNAMSIZ(16) and like this tools already take care
				 * this case then kernel don't support ghost ops.
				 */
				case (EINVAL) :
					fprintf(stderr, "ethghost: Error: couldn't ghostify interface; are you sure that your kernel supports Ghostification?\n");
					break;
				/*
				 * This error code is send by the ghostification kernel code
				 * if the specified interface exist and is already ghositifed.
				 */
				case (EEXIST) :
					fprintf(stderr, "ethghost: Error: the specified interface is already ghostified.\n");
					break;
				/*
				 * This error code is send by the ghostification kernel code
				 * if the specified interface (really) doesn't exist.
				 */
				case (ENODEV) :
					fprintf(stderr, "ethghost: Error: the specified interface doesn't exist (ghostify).\n");
					break;
				/*
				 * This error code is send by the ghostification kernel code if the
				 * specified interface exist but it cann't be ghositfied because the
				 * maximum number of interface ghostified has already been reached.
				 */
				case (ENOMEM) :
					fprintf(stderr, "ethghost: Error: the maximum number of ghostified interfaces has been reached.\n");
					break;
				/*
				 * A unknown error took place (not return but the ghostification
				 * kernel code) so we return a generic message with the original
				 * system error message between parentheses.
				 */
				default :
					fprintf(stderr, "ethghost: Error: an error occurred during ghostification (%s).\n",strerror(nerror));
			}
			break;
		case (EUNGHOSTIFY) :
			dinfo;
			switch (nerror)
			{
				/*
				 * This error code is send by the ghostification kernel code
				 * if the interface specified (really) doesn't exist and so
				 * it cann't be ghostified.
				 */
				case (ENODEV) :
					fprintf(stderr, "ethghost: Error: the specified interface doesn't exist (unghostify).\n");
					break;
				/*
				 * This error occurs when the specified interface is not
				 * ghostified (but it exists)
				 */
				case (ESRCH) :
					fprintf(stderr, "ethghost: Error: the specified interface isn't ghostified.\n");
					break;
				/*
				 * This error code cann't be sent by the ghostification kernel
				 * code and arguments of the ioctl request should therefore be
				 * valid then certainly it is the kernel which does not support
				 * ghostification operations.
				 */
				case (EINVAL) :
					fprintf(stderr, "ethghost: Error: couldn't unghostify interface; are you sure that your kernel supports Ghostification?\n");
					break;
				/*
				 * A unknown error took place (not return but the ghostification
				 * kernel code) so we return a generic message with the original
				 * system error message between parentheses.
				 */
				default :
					fprintf(stderr, "ethghost: Error: an error occurred during unghostification (%s).\n",strerror(nerror));
			}
			break;
		default :
			dinfo;
			fprintf(stderr, "ethghost: Error: an unexpected error took place (EBUG).\n");
			return (EBUG);
	}
	return (nerror);
}

/*
 * Create socket to {ghostify,unghostify}_iface, this socket will be
 * used as file descriptor (*sk) to the ioctl request, this function
 * returns EXIT_SUCCESS on success, errno on error.
 */
static unsigned int __create_socket (int *sk)
{
	errno = 0;
	dinfo;
    if ((*sk = socket(AF_INET,SOCK_DGRAM,0)) < 0) {
		return (on_error(ESOCKCREATE, errno));
    }
	return (EXIT_SUCCESS);
}

/*
 * Destroy socket (*sk) which has been created by the function
 * __create_socket, this function returns EXIT_SUCCESS on success,
 * errno on error.
 */
static unsigned int __destroy_socket (int *sk)
{
	errno = 0;
	dinfo;
	if ((close((int)*sk) < 0)) {
		return (on_error(ESOCKDELETE, errno));
	}
	return (EXIT_SUCCESS);
}

/*
 * Function used to Ghostify an interface (iface) by using ioctl
 * request, return EXIT_SUCCESS on success and errno on error.
 */
static unsigned int __ghostify (int *sk, const char *iface)
{
	errno = 0;
	dinfo;
	if ((ioctl(*sk, SIOCGIFGHOSTIFY, iface)) < 0 ) {
		return (on_error(EGHOSTIFY, errno));
	}
	return (EXIT_SUCCESS);
}

/*
 * Function used to UnGhostify an interface (iface) by using ioctl
 * request, return EXIT_SUCCESS on success and errno on error.
 */
static unsigned int __unghostify (int *sk, const char *iface)
{
	errno = 0;
	dinfo;
	if ((ioctl(*sk, SIOCGIFUNGHOSTIFY, iface)) < 0 ) {
		return (on_error(EUNGHOSTIFY, errno));
	}
	return (EXIT_SUCCESS);
}

/*
 * Function ghostify_iface, used to ghotify an interface, call internally
 * __create_socket to get a file descriptor, call __ghostify to make an
 * ioctl request and ghostify iface (if kenrel support Ghostification)
 * and finally call __destroy_socket. Return errno provide by a funtion
 * call internally on error or EXIT_SUCCESS on success.
 */
unsigned int ghostify_iface (const char *iface)
{
	int sk = 0;
	int error = 0;
	int errorp = 0;

	/* debug */
	dinfo;

	/* 1) create socket */
	if ((error = __create_socket(&sk)) != 0 ) {
		fprintf(stderr, "ethghost: Error: in %s , Exit!!\n",__FUNCTION__);
		return error;
	}

	/* debug */
	dprintf("Socket created with success, goto __ghostify");
	/* 2) ghostify iface */
	if ((error =__ghostify(&sk, iface)) != 0) {
		fprintf(stderr, "ethghost: Error: in %s : interface %s, Exit!!\n",__FUNCTION__,iface);
		/* to preserve original error (if possible) */
		errorp = __destroy_socket(&sk);
		return errorp ? errorp : error;
	}

	/* 3) destroy socket*/
	if ((error = __destroy_socket(&sk)) != 0 ) {
		fprintf(stderr, "ethghost: Error: in %s , Exit!!\n",__FUNCTION__);
	}

	/* debug */
	dprintf("Socket deleted with success, goto main");

	/* return error to main for user (0 on succes) */
	return (error);
}

/*
 * Function unghostify_iface, used to unghotify an interface, call internally
 * __create_socket to get a file descriptor, call __unghostify to make an
 * ioctl request and unghostify iface (if kenrel support Ghostification)
 * and finally call __destroy_socket. Return errno provide by a funtion
 * call internally on error or EXIT_SUCCESS on success.
 */
unsigned int unghostify_iface (const char *iface)
{
	int sk = 0;
	int error = 0;
	int errorp = 0;

	/* debug */
	dinfo;

	/* 1) create socket */
	if ((error = __create_socket(&sk)) != 0 ) {
		fprintf(stderr, "ethghost: Error: in %s , Exit!!\n",__FUNCTION__);
		return error;
	}

	/* debug */
	dprintf("Socket created with success, goto __unghostify");
	/* 2) unghostify iface */
	if ((error =__unghostify(&sk, iface)) != 0) {
		fprintf(stderr, "ethghost: Error: in %s about the interface %s. Exit!!\n",__FUNCTION__,iface);
		/* to preserve original error (if possible) */
		errorp = __destroy_socket(&sk);
		return errorp ? errorp : error;
	}

	/* 3) destroy socket*/
	if ((error = __destroy_socket(&sk)) != 0 ) {
		fprintf(stderr, "ethghost: Error: in %s. Exit!!\n",__FUNCTION__);
	}

	/* debug */
	dprintf("Socket deleted with success, goto main");
	/* return error to main for user (0 on succes) */
	return (error);
}

