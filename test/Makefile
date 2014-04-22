.SUFFIXES: .F90 .f90 .o .a
include ../arch.make

.PHONY: default
default: all

# The different libraries
OBJS = test_ncdf.o

INC += -I../lib/fvar
INC += -I../src

LIB = ../src/libncdf.a
LIBVARDICT  = ../lib/fvar/libvardict.a

.PHONY: all

TESTS = test_ncdf3
all: $(TESTS)

test_ncdf3: test_ncdf.o
	$(FC) -o $@ $(FFLAGS) $^ $(LIB) $(LIBVARDICT) $(LDFLAGS)
	./$@
#mpirun -np 2 ./$@

.PHONY: clean
clean:
	-rm -f *.o *.mod
	-rm -f test_ncdf

