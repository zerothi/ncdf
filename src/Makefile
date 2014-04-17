.SUFFIXES: .f90 .o .a
include ../arch.make

.PHONY: default
default: lib

# The different libraries
OBJS = nf_ncdf.o

INC += -I../lib/fvar

LIB = libncdf.a
LIBVARDICT  = ../lib/fvar/libvardict.a

.PHONY: lib
lib: $(LIBVARDICT) $(LIB)

$(LIB): $(LIBVARDICT) $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(LIBVARDICT) $(OBJS)
	$(RANLIB) $(LIB)

$(LIBVARDICT):
	@echo "*** Ensure succesful compilation of $(LIBVARDICT) ***"
	exit 1

.PHONY: prep
SED_DEL = 's/NEWLINE/\n/g;/^$$/d;/^\!.*&/d;\
s/[[:space:]]*\#\#[[:space:]]*\([^[:space:]]*\)/\1/g;\
s/[[:space:]]*\#\([^i][^[:space:]]*\)/"\1"/g;\
s/"endif"/\n\#endif/g'
prep:
	./ncdf.sh
	$(PP) nf_ncdf.F90 | sed -e $(SED_DEL) > tmp.F90 #2> /dev/null
	$(PP) tmp.F90 | sed -e $(SED_DEL) > nf_ncdf.f90 #2> /dev/null

.PHONY: clean
clean:
	-rm -f $(OBJS) $(LIB) $(LIBVARDICT) *.o *.mod
	-rm -f ncdf_interface.inc ncdf_funcs.inc

# Dependencies
nf_ncdf.o: | prep
nf_ncdf.o: $(LIBVARDICT)
