
include arch.make

.PHONY: default
default: all

.PHONY: all
all: ncdf
	@echo Done with everything

.PHONY: ncdf
ncdf:
	(cd lib/fvar ; make lib)
	(cd src ; make lib)

.PHONY: test
test: ncdf
	(cd test ; make all)

.PHONY: clean
clean:
	(cd lib/fvar ; make clean)
	(cd src ; make clean)
	(cd test ; make clean)
