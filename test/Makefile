.SUFFIXES: .F90 .f90 .o .a

ARCH_MAKE_DEFAULT=../arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# Setup the default things, in case they haven't been set.
AR         ?= ar
ARFLAGS    ?= cru
RANLIB     ?= ranlib
PP         ?= cpp -E -P -C
LIBVARDICT ?= ../lib/fvar/libvardict.a

# The VPATH can be different, however the tree-structure
# has to be the same in this case.
VPATH?=$(shell pwd)

.PHONY: default
default: all

# library for tests
TST_UTILS = tst_ncdf_utils.o
LIB_TST   = libtst.a

$(LIB_TST): $(TST_UTILS)
	$(AR) $(ARFLAGS) $(LIB_TST) $(TST_UTILS)
	$(RANLIB) $(LIB_TST)

# The different libraries
OBJS = test_ncdf.o

LIB = ../src/libncdf.a $(LIB_TST)

# We need the local installation mods
INC += -I../src

.PHONY: all

TESTS  = tst_ncdf_3 tst_ncdf_4 tst_ncdf_att
TESTS += tst_ncdf_crt

.PHONY: $(TESTS)
all: $(LIB_TST) $(TESTS)

tst_ncdf_3: tst_ncdf_3.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
#mpirun -np 2 ./$@

tst_ncdf_4: tst_ncdf_4.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
	mpirun -np 2 ./$@

tst_ncdf_att: tst_ncdf_att.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
	mpirun -np 2 ./$@

# This should not run in parallel, however, it could be performed.
tst_ncdf_crt: tst_ncdf_crt.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@


.PHONY: clean
clean:
	-rm -f *.o *.mod $(LIB_TST) $(TESTS)
	-rm -f *.nc
