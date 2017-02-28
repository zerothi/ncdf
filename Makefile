# Define VPATH
VPATH ?= $(shell pwd)

# Define default target:
.PHONY: default
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



# Define internal fdict library build
.PHONY: prep-fdict lib-fdict clean-fdict
prep-fdict:
	(mkdir -p fdict/obj ; cd fdict/obj ; \
		echo "TOP_DIR =../../$(TOP_DIR)/fdict" > Makefile ; \
		echo "include ../../$(TOP_DIR)/fdict/Makefile" >> Makefile )
lib-fdict: prep-fdict
ifdef SETUP
	$(MAKE) -C fdict/obj SETUP=../../$(SETUP)
else
	$(MAKE) -C fdict/obj
endif
install-fdict:
ifdef SETUP
	$(MAKE) -C fdict/obj SETUP=../../$(SETUP) $(MAKECMDGOALS)
else
	$(MAKE) -C fdict/obj $(MAKECMDGOALS)
endif
clean-fdict: prep-fdict
ifdef SETUP
	$(MAKE) -C fdict/obj SETUP=../../$(SETUP) clean
else
	$(MAKE) -C fdict/obj clean
endif



# Define options for compilation
#### MPI
MPI ?= 0
ifneq ($(MPI),0)
 # Define a parallel NCDF
 FPPFLAGS += -DNCDF_PARALLEL
endif

ifneq (,$(findstring NCDF_PARALLEL,$(FPPFLAGS)))
 MPI=1
endif

#### NetCDF-4 API
CDF ?= 3
ifeq ($(CDF),4)
 # Define NetCDF with _4 support
 FPPFLAGS += -DNCDF_4
endif

# Force if set by the user
ifneq (,$(findstring NCDF_4,$(FPPFLAGS)))
 CDF = 4
endif


# Include the makefile in the src directory
include $(TOP_DIR)/src/Makefile.inc



# The linker is a fortran compiler
LINK := $(FC)


# Figure out if the fdict library is linked manually.
# In this case we do not add dependency on the 
ifeq (,$(findstring fdict,$(LIBS)))
 # In this instance the fdict library is build internally

 # Not found, we need to create correct dependency.
 $(OBJECTS): lib-fdict
 $(NCDF_LIB_STATIC): | lib-fdict
 $(NCDF_LIB_SHARED): | lib-fdict

# Add clean-target dependency
clean: clean-fdict
install: install-fdict

 # Now create the correct linker flags etc. for internal linking
 INCLUDES += -Ifdict/obj
 # For tests
 LIBS += -Lfdict/obj -lfdict

 # When using the internal library we force the copy (the user
 # *must* not change the settings)
settings.bash: lib-fdict FORCE
	-cp fdict/obj/settings.bash .

else
 # In this instance the fdict library is supplied externally
 # The variable FDICT_PREFIX should be present
 ifdef FDICT_PREFIX
settings.bash: FORCE
	-cp $(FDICT_PREFIX)/bin/settings.bash .
 else
$(info ncdf-build:)
$(info ncdf-build: FDICT_PREFIX is not defined, we default to the internal one)
$(info ncdf-build:)
settings.bash:
	VPATH=$(TOP_DIR) $(TOP_DIR)/setup.sh --default
 endif

endif


# This handy target copies from the SOURCES_DIR all sources
# to the current directory
# But ONLY if the current directory is not the top of the project
.PHONY: copy
ifeq ($(TOP_DIR),.)
copy:
	@echo ""
	@echo "make copy does not work when executed from the top ncdf directory"
	@echo "Please create an object directory with an appropriate Makefile"
	@echo ""
else
copy:
	cp $(SOURCES_DIR)/src/*.f90 .
endif


# Create source target for creating _only_ the sources.
.PHONY: source
source: source-src

# Dependent on the option we can fake a VPATH to contain
# any pre-created sources, if they exist we can simply use those
SOURCES_DIR = $(TOP_DIR)/sources
ifeq ($(CDF),4)
 SOURCES_DIR := $(SOURCES_DIR)_4
endif
ifneq ($(MPI),0)
 SOURCES_DIR := $(SOURCES_DIR)_parallel
endif

# Libraries depend on the objects
ifneq ($(LIBRARIES),)
$(LIBRARIES): $(OBJECTS)

# Create target
.PHONY: lib
lib: settings.bash $(LIBRARIES)

endif


# Include the makefile in the test directory
include $(TOP_DIR)/test/Makefile.inc

