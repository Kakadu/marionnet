%define name marionnet-fs-routers-default
summary:  virtual routers for marionnet
Name: %{name}
Version: %{version}
Release: 1
Source: %{name}-%{version}.tar.gz
Vendor: Jean-Vicent Loddo and Luca Saiu - LIPN - Universite Paris 13
Packager: Franck Butelle <Franck.Butelle@lipn.fr>
URL: http://www.marionnet.org
License: GPL
Group: Emulators
BuildRoot: %{_tmppath}/%{name}-%{version}

Provides: marionnet-routers-default
Requires: marionnet-common

%description
Default virtual machine for routers (debian linux 2.6.18) used by marionnet.

%prep
%setup

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/local/share/marionnet/filesystems
cp -a --sparse=auto share/filesystems/router* $RPM_BUILD_ROOT/usr/local/share/marionnet/filesystems

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/local/share/marionnet/filesystems/router*

%changelog
  * Sun Nov 08 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- rename router -> routers, added symlinks
  * Wed Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- division in common, kernel-default, fs-machine-default, fs-router-default
  * Sun Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
        - Initial version of the spec file

