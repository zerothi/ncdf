#!/bin/bash

# Run the setup in the lib/fdict directory
pushd lib/fdict 2&>/dev/null
./setup.sh $@
popd 2&>/dev/null
