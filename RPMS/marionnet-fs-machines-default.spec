%define name marionnet-fs-machines-default

summary:  virtual machines (debian and pinocchio) for marionnet
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

Provides: marionnet-machines-default
Requires: marionnet-common

%description
Default virtual machines (debian linux 2.6.18 and Pinocchio) for marionnet.

%prep
%setup

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/local/share/marionnet/filesystems
echo `pwd`
cp -a --sparse=auto share/filesystems/machine* $RPM_BUILD_ROOT/usr/local/share/marionnet/filesystems

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/local/share/marionnet/filesystems/machine*

%changelog
  * Sun Nov 09 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- renamed machine ->machines, added symlinks
  * Wed Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- division in common, kernel-default, fs-machine-default, fs-router-default
  * Sun Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
        - Initial version of the spec file

