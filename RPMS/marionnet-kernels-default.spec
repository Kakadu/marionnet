%define name marionnet-kernels-default

summary:  UML kernels for marionnet
Name: %{name}
Version: %{version}
Release: 1
Source: %{name}-%{version}.tar.gz
Vendor: Jean-Vicent Loddo and Luca Saiu - LIPN - Universite Paris 13
Packager: Franck Butelle <Franck.Butelle@lipn.fr>
URL: http://www.marionnet.org/download/snapshots/stuff/
License: GPL v2+
Group: Emulators
BuildRoot: %{_tmppath}/%{name}-%{version}

Provides: marionnet-kernels-default
Requires: marionnet-common

%description
The default UML kernels for marionnet (based on debian linux 2.6.18).

%prep
%setup

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/local/share/marionnet/kernels
cp -a share/kernels $RPM_BUILD_ROOT/usr/local/share/marionnet

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/local/share/marionnet/kernels/*

%changelog
  * Sun Nov 08 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- renamed kernels-default, added symlinks.
  * Wed Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
	- division in common, kernel-default, fs-machine-default, fs-router-default
  * Sun Jun 06 2009 F. BUTELLE <Franck.Butelle@lipn.fr>
        - Initial version of the spec file

