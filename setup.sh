#!/bin/bash

# Run the setup in the lib/fvar directory
pushd lib/fvar 2&>/dev/null
./setup.sh $@
popd 2&>/dev/null
