#!/bin/bash -x

# This file is part of Marionnet, a virtual network laboratory
# Copyright (C) 2007  Jean-Vincent Loddo
# Copyright (C) 2007  Luca Saiu

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

# Requires:
#   1. brctl 
#   2. mii-tool 
#   3. iwconfig
#   4. timeout  (package timeout, required for dhclient)

function require {
 local PROG
 for PROG in "$@"; do
  which $PROG >/dev/null || { echo "$PROG not found in the PATH, is it installed?" 1>&2; exit 1; }
 done
}

# Check requirements
require brctl mii-tool iwconfig killall dhclient mktemp route


#####################
#      TOOLS        #
#####################
 
# Dump of a configured network interface
function ifconfig_dump {
 local IF="$1"

 ifconfig "$IF" | \
  head -n 4 | \
  sed 's/HWaddr /HWaddr:/ ; s/6 addr: /_ipv6:/'  | \
  tr ' ' '\n' | \
  sed 's/^addr:/IPV4:/'  | \
  grep '^[a-zA-Z0-9_]*:' | \
  sed 's/\(^[a-zA-Z0-9_]*\):/\1=/' | \
  awk -F= '/^HWaddr/     {print "MAC="$2}
           /^Bcast=/     {print "BROADCAST="$2}
           /^IPV4=/      {print "IP="$2}
           /^Mask=/      {print "NETMASK="$2}
           /^MTU=/       {print "MTU="$2}
           /^inet_ipv6=/ {print "IPV6="$2}'

} 


# Get the broadcast address of a given interface (which we suppose to be up)
function broadcast_of {
 local IF=$1
 ifconfig $IF | grep 'inet addr' | tr : ' ' | awk '{print $5}'
}

# Try to kill a dhclient launched for the given interface, else kill all dhclients
function kill_dhclient {
 local IF=$1
 local PID=$(ps aux | grep dhclient | grep $IF | awk '{print $2}')
 if [[ -n "$PID" ]]; then
  kill -9 $PID
 else
  killall dhclient
 fi
}


#####################
#    STD POLICY     #
#####################


# The standard policy consists in detecting a "main" interface searching for
# the interface connected to the default gateway, and in configuring the bridge
# exactly like that (and in unconfiguring the old main interface). 
function standard_policy {
 
 local TMPFILE=$(mktemp /tmp/prepare_bridge.XXXXX)
 local IFS=$(route -n | grep '^0[.]0[.]0[.]0' | tee "$TMPFILE" | awk '{print $NF}')
 local LINES=$(wc -l "$TMPFILE" | cut -d" " -f1)
 
 # If there are less or more than 1 line, this policy fails
 [[ $LINES = 1 ]] || { 
   echo "The standard policy is not appliable" >&2; 
   return 1; 
   }
 
 local GW=$(awk <"$TMPFILE" '{print $2}')
 local IF=${IFS}

 # Add the unique candidate into the bridge
 brctl addif "$BRIDGE" "$IF" || { 
   echo "The unique candidate cannot be plugged into the bridge" >&2; 
   return 1; 
   }
 
 # Bridge up
 ifconfig "$BRIDGE" up

 # Configure the bridge in the same way as the "main" interface ($IF)

 ifconfig_dump "$IF" >"$TMPFILE"
 source "$TMPFILE" # now we have MAC IP BROADCAST NETMASK IPV6 MTU

 ifconfig "$BRIDGE" "$IP" broadcast "$BROADCAST" netmask "$NETMASK"
 ifconfig "$BRIDGE" mtu "$MTU"
 [[ -n "$IPV6" ]] && ifconfig "$BRIDGE" inet6 add "$IPV6"

 # Unconfigure the main interface
 ifconfig "$IF" 0.0.0.0 promisc up 
 [[ -n "$IPV6" ]] && ifconfig "$IF" inet6 del "$IPV6" 

 # Default route
 route add default gw "$GW" "$BRIDGE"

 # Mr proper
 rm -f "$TMPFILE"
 
 return 0  
}
# end of standard_policy() 




#####################
#  FALLBACK POLICY  #
#####################


# The fallback policy consists in scanning all running interfaces, putting them
# into the bridge, configuring the bridge with DHCP, and unconfiguring the "main" one. 
function fallback_policy {

 # Search for the candidates
 local LIST1=$(mii-tool | grep '^[a-zA-Z0-9_]*:' | cut -d: -f1)
 local LIST2=$(iwconfig 2>&1 | grep -v 'no wireless' | grep  '^[a-zA-Z0-9_]' | cut -d" " -f1)
 local LIST=$(echo $LIST1 $LIST2 | tr " " "\n" | sort | uniq)

 # Add interface(s) in the bridge
 for IF in $LIST; do
  # Interfaces can be added even if up
  brctl addif $BRIDGE $IF || true
 done

 # Bridge up
 ifconfig $BRIDGE up

 # Configure the bridge with a dhcp client
 timeout 45 dhclient $BRIDGE || {
   echo "The bridge cannot be configured with DHCP" >&2; 
   return 1; 
   }

 # Now remove the IP configuration from the interface configured with
 # the same broadcast address of the bridge, in order to prevent conflicts
 # in the routing table

 local BC_BRIDGE=$(broadcast_of $BRIDGE)
 local BC 

 for IF in $LIST; do
  BC=$(broadcast_of $IF)
  [[ "$BC" = "$BC_BRIDGE" ]] && ifconfig $IF 0.0.0.0 promisc up && kill_dhclient $IF
 done

} 
# end of fallback_policy() 


#####################
#      M A I N      #
#####################

# The name of the unique bridge for Marionnet (several instances will share it)
BRIDGE=br0

# If the script is called with 'stop' delete the bridge from the system.
# Note that the user has to manually reconfigure the included interfaces. 
if [[ "$1" = stop ]]; then 
 ifconfig "$BRIDGE" down 
 brctl delbr "$BRIDGE"
 exit 0
fi

# If '$BRIDGE' exists and is already configured, exit with success
ifconfig "$BRIDGE" &>/dev/null && {
 TMPFILE=$(mktemp /tmp/prepare_bridge.XXXXX)
 ifconfig_dump "$BRIDGE" >"$TMPFILE"
 source "$TMPFILE" # now we have MAC IP BROADCAST NETMASK IPV6 MTU
 rm -f "$TMPFILE"
 [[ -n "$IP" ]] && exit 0
 # If the bridge is not well configured, destroy it and continue (it will be re-created)
 unset MAC IP BROADCAST NETMASK IPV6 MTU
 ifconfig "$BRIDGE" down 
 brctl delbr "$BRIDGE"
 }

# Define the bridge if necessary
brctl addbr "$BRIDGE" 2>/dev/null || true

# Choice of policy
standard_policy || fallback_policy || {
   echo "No policy could be applied, the ethernet socket will"
   echo "not be functional in Marionnet"  
   exit 1 
   } >&2

