.SUFFIXES: .F90 .f90 .o .a
include ../arch.make

.PHONY: default
default: all

# library for tests
TST_UTILS = tst_ncdf_utils.o
LIB_TST = libtst.a

$(LIB_TST): $(TST_UTILS)
	$(AR) $(ARFLAGS) $(LIB_TST) $(TST_UTILS)
	$(RANLIB) $(LIB_TST)

# The different libraries
OBJS = test_ncdf.o

INC += -I../lib/fvar
INC += -I../src

LIB = ../src/libncdf.a $(LIB_TST)
LIBVARDICT  = ../lib/fvar/libvardict.a

.PHONY: all

TESTS = tst_ncdf_3 tst_ncdf_4
all: $(LIB_TST) $(TESTS)

tst_ncdf_3: tst_ncdf_3.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
	#mpirun -np 2 ./$@

tst_ncdf_4: tst_ncdf_4.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
	mpirun -np 2 ./$@

.PHONY: clean
clean:
	-rm -f *.o *.mod $(LIB_TST) tst_ncdf_3

