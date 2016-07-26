# Define VPATH
VPATH ?= $(shell pwd)

# Define default target:
default: lib

# SMEKASETTINGS (DO NOT DELETE)
# DO NOT CHANGE CONTENT IN THIS BLOCK
# IT MAY BE OVERWRITTEN WHEN REINSTALLING SMEKA
#
# This Makefile was created by smeka:
#  github.com/zerothi/smeka

# Top-directory of Makefile/source tree
# If need set, do so ABOVE this block!
TOP_DIR ?= .

# Directory of smeka default Makefiles
SMEKA_DIR = smeka

# Include the smeka settings!
include $(TOP_DIR)/$(SMEKA_DIR)/Makefile.smeka

# SMEKAENDSETTINGS (DO NOT DELETE)

ifeq ($(NCDF_FDICT_LOCAL), 1)

#    If it is not found define the appropriate
#    LIBS and LDFLAGS
LIBS += $(VPATH)/fdict/libvardict.a
INCLUDES += -I$(VPATH)/fdict/src

endif

# Include the makefile in the src directory
include src/Makefile.inc

# Libraries depend on the objects
$(LIBRARIES): $(OBJECTS)

# Create target
lib: settings.bash $(LIBRARIES)

# Include the makefile in the test directory
include test/Makefile.inc

