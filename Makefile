
# Set the default VPATH
VPATH?=$(shell pwd)

ARCH_MAKE_DEFAULT=arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# We need to assure that libvardict.a is existing

.PHONY: default
default: all

.PHONY: all
all: ncdf
	@echo Done with everything

.PHONY: ncdf
ncdf:
ifdef LIBVARDICT
	@echo "Using pre-built LIBVARDICT: $(LIBVARDICT)"
else
	(cd lib/fvar ; make "VPATH=$(VPATH)/lib/fvar" lib)
endif
	(cd src ; make "VPATH=$(VPATH)/src" lib)

.PHONY: test
test: ncdf
	(cd test ; make "VPATH=$(VPATH)/test" all)

.PHONY: clean
clean:
ifndef LIBVARDICT
	(cd lib/fvar ; make clean)
endif
	(cd src ; make clean)
	(cd test ; make clean)
