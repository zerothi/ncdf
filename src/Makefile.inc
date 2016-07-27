# Define src-directory
src_DIR = $(TOP_DIR)/src
VPATH := $(VPATH):$(src_DIR)


# Define sources in this src directory
src_CPP_SOURCES = ncdf.f90
src_SOURCES = $(src_CPP_SOURCES)
src_OBJECTS = $(patsubst %.f90, %.o, $(src_SOURCES))


# Create dependency tree such that we may bypass phony targets
ncdf.o: ncdf.f90
ncdf.f90: $(addprefix ncdf, .sh _att_inc.inc _var_inc.inc _pp.F90)
	cd $(src_DIR) && ./ncdf.sh
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) $(src_DIR)/ncdf_pp.F90 \
		| sed -f $(src_DIR)/filter.sed > ncdf.f90 $(PIPE_SILENT)


.PHONY: clean-src
clean-src:
	-rm -f *.o *.mod ncdf.f90
	-rm -f $(addprefix $(src_DIR)/, *.o *.mod)
	-rm -f $(call addpresuf,$(src_DIR)/ncdf_,.inc, funcs interface)

clean: clean-src


# Define objects that needs compilation
SOURCES += $(src_SOURCES)
CPP_SOURCES += $(src_CPP_SOURCES)
F_MODS  += $(patsubst %.f90, %.mod, $(src_SOURCES))
OBJECTS += $(src_OBJECTS)

# Ensure that the objects are built with the correct module path
$(src_OBJECTS): INCLUDES += -I$(src_DIR)


# Local Variables:
#  mode: makefile-gmake
# End:
