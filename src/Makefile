include ../arch.make

.PHONY: default
default: libs

# The different libraries
NCDF_OBJS = ncdf.o
VAR_OBJS  = var.o
DICT_OBJS = dictionary.o

LIBNCDF = libncdf.a
LIBVAR  = libvar.a
LIBDICT = libdictionary.a
LIB     = libnpa.a

.PHONY: libs
libs: $(LIBVAR) $(LIBDICT) $(LIBNCDF)
	$(AR) $(ARFLAGS) $(LIB) $(LIBVAR) $(LIBDICT) $(LIBNCDF)
	$(RANLIB) $(LIB)
	
$(LIBNCDF): $(LIBVAR) $(LIBDICT) $(NCDF_OBJS)
	$(AR) $(ARFLAGS) $(LIBNCDF) $(LIBVAR) $(LIBDICT) $(NCDF_OBJS)
	$(RANLIB) $(LIBNCDF)

$(LIBDICT): $(LIBVAR) $(DICT_OBJS)
	$(AR) $(ARFLAGS) $(LIBDICT) $(LIBVAR) $(DICT_OBJS)
	$(RANLIB) $(LIBDICT)

$(LIBVAR): $(VAR_OBJS)
	$(AR) $(ARFLAGS) $(LIBVAR) $(VAR_OBJS)
	$(RANLIB) $(LIBVAR)

.PHONY: prep_var
prep_var:
	./var.sh

.PHONY: prep_ncdf
prep_ncdf:
	./ncdf.sh

.PHONY: prep_dict
prep_dict:
	./dictionary.sh

# Dependencies
var.o: prep_var
ncdf.o: prep_ncdf var.o dictionary.o
dictionary.o: prep_dict var.o
