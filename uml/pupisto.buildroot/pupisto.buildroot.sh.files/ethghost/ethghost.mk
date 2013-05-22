##############################
#  Ghostification (userland) #
##############################
#
# TARGET_CC is the C compiler used for cross compilation
# TARGET_LD same than TARGET_CC
# @D is the current directoryin which package has been uncompress
# STAGING_DIR = By default, the value is DESTDIR=$(STAGING_DIR) install

ETHGHOST_VERSION = 2.0
ETHGHOST_SOURCE = ethghost-$(ETHGHOST_VERSION).tar.gz
ETHGHOST_SITE = http://www.marionnet.org/downloads/ethghost/$(ETHGHOST_SOURCE)
ETHGHOST_INSTALL_STAGING = YES

define ETHGHOST_BUILD_CMDS
	$(MAKE) CC="$(TARGET_CC)" LD="$(TARGET_LD)" -C $(@D) all
endef

define ETHGHOST_INSTALL_STAGING_CMDS
	$(MAKE) DESTDIR=$(STAGING_DIR) -C $(@D) install
endef

define ETHGHOST_INSTALL_TARGET_CMDS
	$(INSTALL) -D -m 0755 $(@D)/ethghost \
			      $(TARGET_DIR)/bin/ethghost
endef

# +============+=====+=======+=====+==========+=============+=========+=======+===========+
# |  username  | uid | group | gid | password |   home      | shell   |groups |  comment  |
# +============+=====+=======+=====+==========+=============+=========+=======+===========+

# Non funzionano i seguenti comandi:
# student -1 users 100 a8I77tmxmjkXw /home/student /bin/bash sudo User
# student 1001 users 100 a8I77tmxmjkXw /home/student /bin/bash - Marionnet user
# Errore nello script `support/scripts/mkusers'
# awk -F: -v group=sudo '$1 == group { printf( "%d\n", $3 ); }' $BUILDROOT/output/target/etc/group
# recupera '1001\n27'

define ETHGHOST_USERS
student -1 users 100 a8I77tmxmjkXw /home/student /bin/bash - Marionnet user
endef

$(eval $(generic-package))
