TEST_DIR = $(TOP_DIR)/test

# Define common codes used for the tests
TEST_COMMON = tst_ncdf_utils.o
TEST_COMMON := $(addprefix $(TEST_DIR)/, $(TEST_COMMON))

TEST_EXE_  = tst_ncdf_3 tst_ncdf_4 tst_ncdf_att
TEST_EXE_ += tst_ncdf_crt

TEST_EXE ?= $(TEST_EXE_)

ifeq ($(STATIC),1)
  TEST_LIB    = libncdf.a
else
  ifeq ($(SHARED),1)
    TEST_LIB  = libncdf.so
  else
    $(error No library is build, the tests cannot be performed.)
  endif
endif

# Create correct dependency schemes without adding too
# many flags in the direct compilation targets.
# Ensure the common utility is compiled before any test
$(call addpresuf ,$(TEST_DIR)/,.o,$(TEST_EXE)): $(TEST_COMMON)
# Ensure the executables depend on the utility and the library
$(TEST_EXE): $(TEST_COMMON) $(TEST_LIB)
# Ensure the src/ directory is included in the search path
# for modules
$(TEST_EXE): INCLUDES += -I$(VSRC_DIR)
# Ensure the test directory gets the modules
$(TEST_EXE): FFLAGS += $(FC_MODDIR)$(TEST_DIR)


# Add source includes

.PHONY: test
.NOTPARALLEL: test
test: $(TEST_LIB) $(TEST_EXE)


tst_ncdf_3: $(TEST_DIR)/tst_ncdf_3.o
	$(FC) -o $(TEST_DIR)/$@ $(FFLAGS) $(INCLUDES) $^ $(LIBS) $(LDFLAGS)
	$(TEST_DIR)/$@
ifeq ($(NCDF_MPI),1)
	mpirun -np 2 $(TEST_DIR)/$@
endif

tst_ncdf_4: $(TEST_DIR)/tst_ncdf_4.o
	$(FC) -o $(TEST_DIR)/$@ $(FFLAGS) $(INCLUDES) $^ $(LIBS) $(LDFLAGS)
	$(TEST_DIR)/$@
ifeq ($(NCDF_MPI),1)
	mpirun -np 2 $(TEST_DIR)/$@
endif

tst_ncdf_att: $(TEST_DIR)/tst_ncdf_att.o
	$(FC) -o $(TEST_DIR)/$@ $(FFLAGS) $(INCLUDES) $^ $(LIBS) $(LDFLAGS)
	$(TEST_DIR)/$@
ifeq ($(NCDF_MPI),1)
	mpirun -np 2 $(TEST_DIR)/$@
endif

# This should not run in parallel, however, it could be performed.
tst_ncdf_crt: $(TEST_DIR)/tst_ncdf_crt.o
	$(FC) -o $(TEST_DIR)/$@ $(FFLAGS) $(INCLUDES) $^ $(LIBS) $(LDFLAGS)
	$(TEST_DIR)/$@



.PHONY: clean-test
clean-test:
	-rm -f $(addprefix $(TEST_DIR)/,$(TEST_EXE) $(LIB_TEST) $(TEST_COMMON) *.o *.mod)
clean: clean-test


# Local Variables:
#  mode: makefile-gmake
# End:
