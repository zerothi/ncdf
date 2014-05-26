include arch.make

# Set the default VPATH
VPATH?=$(shell pwd)

.PHONY: default
default: all

.PHONY: all
all: ncdf
	@echo Done with everything

.PHONY: ncdf
ncdf:
	(cd lib/fvar ; make "VPATH=$(VPATH)/lib/fvar" lib)
	(cd src ; make "VPATH=$(VPATH)/src" lib)

.PHONY: test
test: ncdf
	(cd test ; make "VPATH=$(VPATH)/test" all)

.PHONY: clean
clean:
	(cd lib/fvar ; make clean)
	(cd src ; make clean)
	(cd test ; make clean)
