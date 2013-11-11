
include arch.make

.PHONY: default
default: all

.PHONY: all
all: ncdf
	@echo Done with everything

.PHONY: ncdf
ncdf:
	(cd src ; make libs)

.PHONY: tests
tests: ncdf
	(cd test ; make all)
