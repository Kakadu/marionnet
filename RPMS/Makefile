#DO not use this makefile directly, it must be called by the 
#marionnet root makefile

RPMTOPDIR= $(HOME)/tmp/rpm

FIX_VERSION_RPM = \
     if echo $$version | grep snapshot &> /dev/null; then \
        version="$$version"`date +"%Y%m%d"`; \
     fi

# construct RPM necessary structure and spec file
RPMDIRSTRUCT= \
     echo "Construction of RPM directory Structure";\
     rm -rf $(RPMTOPDIR);\
     mkdir -p $(RPMTOPDIR)/BUILD ;\
     mkdir -p $(RPMTOPDIR)/RPMS ;\
     mkdir -p $(RPMTOPDIR)/SOURCES ;\
     mkdir -p $(RPMTOPDIR)/SPECS ;\
     mkdir -p $(RPMTOPDIR)/SRPMS;\
     echo -e "%define version $$version" > $(RPMTOPDIR)/SPECS/$$name.spec;\
     cat RPMS/$$name.spec >> $(RPMTOPDIR)/SPECS/$$name.spec;\
     rm -rf $(RPMTOPDIR)/_tarball ;\
     mkdir -p $(RPMTOPDIR)/_tarball/$$name-$$version

rpms : rpm-common rpm-fs-machines rpm-fs-routers rpm-kernels
# RPM creation suppose that the directory name is at the format name-version
rpm-common : 
	@($(call READ_META, name, version); \
	$(call FIX_VERSION_RPM); \
	echo $$version;\
	name=$$name-common;\
	$(call RPMDIRSTRUCT);\
	rm -rf _build/* ;\
	echo "Copy files to temporary directory";\
	tar --exclude _darcs --exclude share/filesystems --exclude share/kernels \
	   -cf - . | tar -C $(RPMTOPDIR)/_tarball/$$name-$$version -xf - ; \
	mkdir $(RPMTOPDIR)/_tarball/$$name-$$version/share/filesystems;\
	mkdir $(RPMTOPDIR)/_tarball/$$name-$$version/share/kernels;\
	cd $(RPMTOPDIR)/_tarball ;\
	echo "Creation of tarball common";\
        tar -czf \
	   $(RPMTOPDIR)/SOURCES/$$name-$$version.tar.gz $$name-$$version;\
        echo "Tarball-common constructed." ;\
	echo "RPM common creation.";\
	rpmbuild --sign -bb $(RPMTOPDIR)/SPECS/$$name.spec; \
	mv $(RPMTOPDIR)/RPMS/*/*.rpm ~)

		
rpm-fs-machines :
	@($(call READ_META, name, version); \
	$(call FIX_VERSION_RPM); \
	name=$$name-fs-machines-default;\
	$(call RPMDIRSTRUCT, name, version);\
        rm -rf _build/* ;\
	echo "Creation of tarball filesystems";\
	mkdir -p $(RPMTOPDIR)/_tarball/$$name-$$version/share/filesystems;\
	cp -a --sparse=auto share/filesystems/machine* $(RPMTOPDIR)/_tarball/$$name-$$version/share/filesystems;\
	cd $(RPMTOPDIR)/_tarball;\
	tar --sparse -czvf $(RPMTOPDIR)/SOURCES/$$name-$$version.tar.gz $$name-$$version/share/filesystems/machine*;\
	echo "RPM fs-machines-default creation";\
	rpmbuild -bb $(RPMTOPDIR)/SPECS/$$name.spec;\
	mv $(RPMTOPDIR)/RPMS/*/*.rpm ~)

rpm-fs-routers :
	@($(call READ_META, name, version); \
	$(call FIX_VERSION_RPM); \
	name=$$name-fs-routers-default;\
	$(call RPMDIRSTRUCT, name, version);\
        rm -rf _build/* ;\
	echo "Creation of tarball filesystems for routers";\
	mkdir -p $(RPMTOPDIR)/_tarball/$$name-$$version/share/filesystems;\
	cp -a --sparse=auto share/filesystems/router* $(RPMTOPDIR)/_tarball/$$name-$$version/share/filesystems;\
	cd $(RPMTOPDIR)/_tarball;\
	tar --sparse -czvf $(RPMTOPDIR)/SOURCES/$$name-$$version.tar.gz $$name-$$version/share/filesystems/router*;\
	echo "RPM fs-routers-default creation";\
	rpmbuild --sign -bb $(RPMTOPDIR)/SPECS/$$name.spec;\
	mv $(RPMTOPDIR)/RPMS/*/*.rpm ~)

rpm-kernels :
	@($(call READ_META, name, version); \
	$(call FIX_VERSION_RPM); \
	name=$$name-kernels-default;\
	$(call RPMDIRSTRUCT, name, version);\
        rm -rf _build/* ;\
	echo "Creation of tarball kernel";\
	mkdir -p $(RPMTOPDIR)/_tarball/$$name-$$version/share/kernels;\
	cp -a share/kernels $(RPMTOPDIR)/_tarball/$$name-$$version/share;\
	cd $(RPMTOPDIR)/_tarball;\
	tar -czvf $(RPMTOPDIR)/SOURCES/$$name-$$version.tar.gz $$name-$$version/share/kernels;\
	echo "RPM kernel-default creation";\
	rpmbuild --sign -bb $(RPMTOPDIR)/SPECS/$$name.spec; \
	mv $(RPMTOPDIR)/RPMS/*/*.rpm ~)
