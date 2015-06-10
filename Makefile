
# Set the default VPATH
VPATH ?= $(shell pwd)

ARCH_MAKE_DEFAULT=$(VPATH)/arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# We need to assure that libvardict.a is existing

.PHONY: default
default: all

.PHONY: all
all: lib
	@echo Done with everything

.PHONY: lib
lib:
ifdef LIBVARDICT
	@echo "Using pre-built LIBVARDICT: $(LIBVARDICT)"
else
	$(MAKE) -C fdict "VPATH=$(VPATH)/fdict" \
		"ARCH_MAKE=$(ARCH_MAKE)" lib
endif
	$(MAKE) -C src "VPATH=$(VPATH)/src" lib

.PHONY: test
test: lib
	$(MAKE) -C test "VPATH=$(VPATH)/test" all

.PHONY: clean
clean:
ifndef LIBVARDICT
	-if [ -d fdict ]; then $(MAKE) -C fdict "VPATH=$(VPATH)/fdict" \
		"ARCH_MAKE=$(ARCH_MAKE)" clean ; fi
endif
	$(MAKE) -C src clean
	$(MAKE) -C test clean
