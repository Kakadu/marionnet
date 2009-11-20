%define name marionnet-common

# The following is needed to not call strip on the byte codes...
%define debug_package %{nil}
%define __os_install_post %{nil}

summary:  a virtual network laboratory

Name: %{name}
Version: %{version}
Release: 1
Source0: %{name}-%{version}.tar.gz
Vendor: Jean-Vicent Loddo and Luca Saiu - LIPN - Universite Paris 13
Packager: Franck Butelle <Franck.Butelle@lipn.fr>
URL: http://www.marionnet.org
License: GPL v2+
Group: Emulators
BuildRoot:      %{_tmppath}/%{name}-%{version}

Provides: marionnet marionnet-daemon

Requires: glibc vde2 bridge-utils uml-utilities graphviz
Requires(hint): xterm

%description
It allows users to define, configure and run complex computer networks
without any need for physical setup. Only a single, possibly even
non-networked GNU/Linux host machine is required to simulate a whole
Ethernet network complete with computers, routers, hubs, switchs, cables,
and more. Support is also provided for integrating the virtual network
with the physical host network.  As Marionnet is meant to be used also
by inexperienced people, it features a very intuitive graphical user
interface. Marionnet is written in the mostly functional language OCaml
with just some little bits of C, and depends on User Mode Linux and VDE
for the simulation part.

%prep
%setup

%build
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/etc/sysconfig/network-scripts
sed -i -e 's|^prefix=.*$|prefix='$RPM_BUILD_ROOT/usr/local'|'  \
    -e 's|^configurationprefix=.*$|configurationprefix='$RPM_BUILD_ROOT/etc'|' \
    -e 's|^localeprefix=.*$|localeprefix='$RPM_BUILD_ROOT/usr/share/locale'|' \
    CONFIGME
make install
# to have a desktop link
desktop-file-install --dir=$RPM_BUILD_ROOT/%{_datadir}/applications marionnet.desktop

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc COPYING AUTHORS BUILD-TIME README THANKS
%config /etc/marionnet/marionnet.conf
/usr/local/share/marionnet
/usr/local/bin/marionnet.byte
/usr/local/sbin/marionnet-daemon.byte
/usr/share/locale/*/LC_MESSAGES/marionnet.mo
%{_datadir}/applications/marionnet.desktop

%post
if [ $1 == 1 ]; then #first install
  if [ -e /usr/bin/desktop-file-install ]; then
    desktop-file-install --rebuild-mime-info-cache %{_datadir}/applications/marionnet.desktop
  fi
# try to bring up a bridge, only on wired network interface
  if [ -e /etc/sysconfig/network-scripts/ifcfg-eth0 ]; then
    if grep 'ONBOOT=yes' /etc/sysconfig/network-scripts/ifcfg-eth0 >/dev/null; then
	echo -e "DEVICE=br0\nTYPE=Bridge\nONBOOT=yes" > /etc/sysconfig/network-scripts/ifcfg-br0
	grep 'BOOTPROTO=' /etc/sysconfig/network-scripts/ifcfg-eth0 >> /etc/sysconfig/network-scripts/ifcfg-br0
	grep 'IPADDR=' /etc/sysconfig/network-scripts/ifcfg-eth0 >> /etc/sysconfig/network-scripts/ifcfg-br0
	grep 'NETMASK=' /etc/sysconfig/network-scripts/ifcfg-eth0 >> /etc/sysconfig/network-scripts/ifcfg-br0
	echo 'BRIDGE=br0' >> /etc/sysconfig/network-scripts/ifcfg-eth0
	chmod +x /etc/sysconfig/network-scripts/ifcfg-br0
	/etc/init.d/network restart
    fi
  fi
  if [ ! -e /etc/sysconfig/network-scripts/ifcfg-br0 ]; then
	echo "Bridge not configured, you will not be able to access to physical"
	echo "network from marionnet. See brctl and /etc/marionnet/marionnet.conf"
  fi
  echo "Do not forget to install virtual machine images..."
fi

%postun
if [ $1 = 0 ]; then # uninstall
  if [ -e /etc/sysconfig/network-scripts/ifcfg-br0 ]; then
    rm -f /etc/sysconfig/network-scripts/ifcfg-br0
    sed -i /etc/sysconfig/network-scripts/ifcfg-eth0 -e '/^BRIDGE=br0/d'
    /etc/init.d/network restart
    brctl delif br0 eth0
  fi
  if [ -e /usr/bin/update-desktop-database ]; then
     rm -f %{_datadir}/applications/marionnet.desktop
    /usr/bin/update-desktop-database
  fi
fi

%changelog
  * Sun Nov 08 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- simplification of the spec file, addition of br0 stuff
  * Wed Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- division in common, kernel-default, fs-machine-default, fs-router-default
  * Sun Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
        - Initial version of the spec file

