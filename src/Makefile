.SUFFIXES: .f90 .o .a

# The VPATH can be different, however the tree-structure
# has to be the same in this case.
VPATH?=$(shell pwd)

ARCH_MAKE_DEFAULT=../arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# Setup the default things, in case they haven't been set.
AR         ?= ar
ARFLAGS    ?= cru
RANLIB     ?= ranlib
PP         ?= cpp -E -P -C
LIBVARDICT ?= ../lib/fvar/libvardict.a

.PHONY: default
default: lib

# Add user preprocessor flags
PP += $(FPPFLAGS)
# The different object files for this library.
OBJS = nf_ncdf.o

LIB = libncdf.a

.PHONY: lib
lib: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

SED_DEL = 's/NEWLINE/\n/g;/^$$/d;/^\!.*&/d;\
s/[[:space:]]*\#\#[[:space:]]*\([^[:space:]]*\)/\1/g;\
s/[[:space:]]*\#\([^i][^[:space:]]*\)/"\1"/g;\
s/"endif"/\n\#endif/g'
.PHONY: prep
prep:
	$(VPATH)/ncdf.sh
	$(PP) -I. -I$(VPATH) $(VPATH)/nf_ncdf.F90 | sed -e $(SED_DEL) > tmp.F90 #2> /dev/null
	$(PP) -I. -I$(VPATH) tmp.F90 | sed -e $(SED_DEL) > nf_ncdf.f90 #2> /dev/null

.PHONY: clean
clean:
	-rm -f $(OBJS) $(LIB) *.o *.mod tmp.F90 nf_ncdf.f90
	-rm -f ncdf_interface.inc ncdf_funcs.inc

# Dependencies
nf_ncdf.f90: | prep
nf_ncdf.o: | prep
