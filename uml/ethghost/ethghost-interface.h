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


/*
 * Interface for ghostification
 */

#ifndef _ETHGHOST_INTERFACE_H_
#define _ETHGHOST_INTERFACE_H_

/* Macro debug */
#ifdef GHOST_DEBUG
    #define dinfo printf("DEBUG: file(%s): line(%03d): funct(%s): -- info debug -- \n",basename(__FILE__),__LINE__,__FUNCTION__)
    #define dprintf(msg,args...) printf("DEBUG: file(%s): line(%03d): funct(%s): " msg "\n",basename(__FILE__),__LINE__,__FUNCTION__,##args)
#else
    #define dinfo
    #define dprintf(msg,args...)
#endif

/* some variable */
#define __ETHGHOST_VERSION__   "2.0"   /* version of this soft */

/* see include/linux/sockio.h in the Linux Kernel sources */
#define SIOKLOG             0x894D  /* Write a string to the log     */
#define SIOCGIFGHOSTIFY     0x894E  /* Make a network device 'ghost' */
#define SIOCGIFUNGHOSTIFY   0x894F  /* Make a network device 'unghost' */

/*
 * Internals variables (put here to EBUG in main())
 */
enum {
    ESOCKCREATE,
#define ESOCKCREATE ESOCKCREATE
    ESOCKDELETE,
#define ESOCKDELETE ESOCKDELETE
    EGHOSTIFY,
#define EGHOSTIFY   EGHOSTIFY
    EUNGHOSTIFY,
#define EUNGHOSTIFY EUNGHOSTIFY
    EBUG
#define EBUG        EBUG
};

/*
 * Fonction to ghostify an interface.
 * iface : name of the network interface
 * 		   that you want to ghostify
 */
unsigned int ghostify_iface (const char *iface);

/*
 * Fonction to unghostify an interface.
 * iface : name of the network interface
 * 		   that you want to unghostify
 */
unsigned int unghostify_iface (const char *iface);

#endif /* _ETHGHOST_INTERFACE_H */
