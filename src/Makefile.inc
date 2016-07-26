# Define src-directory
SRC_DIR = $(TOP_DIR)/src
VSRC_DIR = $(VPATH)/$(SRC_DIR)


# Define sources in this src directory
SRC_CPP_SOURCES = $(addprefix $(VSRC_DIR)/,ncdf.f90)
SRC_SOURCES = $(SRC_CPP_SOURCES)
SRC_OBJECTS = $(patsubst %.f90, %.o, $(SRC_SOURCES))


# Create dependency tree such that we may bypass phony targets
$(VSRC_DIR)/ncdf.o: $(VSRC_DIR)/ncdf.f90
$(VSRC_DIR)/ncdf.f90: $(addprefix $(VSRC_DIR)/ncdf, .sh _att_inc.inc _var_inc.inc _pp.F90)
	cd $(VSRC_DIR) ; ./ncdf.sh
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $(INCLUDES) -I$(VSRC_DIR) $(VSRC_DIR)/ncdf_pp.F90 \
		| sed -f $(VSRC_DIR)/filter.sed > $(VSRC_DIR)/ncdf.f90 $(PIPE_SILENT)

.PHONY: clean-src
clean-src:
	-rm -f $(addprefix $(VSRC_DIR)/, *.o *.mod)
	-rm -f $(addprefix $(VSRC_DIR)/, ncdf.f90)
	-rm -f $(call addpresuf,$(VSRC_DIR)/ncdf_,.inc, funcs interface)
clean: clean-src


# Define objects that needs compilation
SOURCES += $(SRC_SOURCES)
CPP_SOURCES += $(SRC_CPP_SOURCES)
F_MODS  += $(patsubst %.f90, %.mod, $(SRC_SOURCES))
OBJECTS += $(SRC_OBJECTS)

# Ensure that the objects are built with the correct module path
$(SRC_OBJECTS): FFLAGS += $(FC_MODDIR)$(VSRC_DIR)

# Local Variables:
#  mode: makefile-gmake
# End:
