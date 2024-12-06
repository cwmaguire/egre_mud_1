#!/usr/bin/env bash

# pull in the latest files from the deps so I don't have to worry
# about patching changes back to the original repos when I make
# changes to the deps/egre{,_mud}/src files. This way I can edit
# ~/dev/egre{,_mud}/src/ files directly.
# (-u means only copy newer files)
cp -u ~/dev/egre/src/* deps/egre/src/
cp -u ~/dev/egre/include/* deps/egre/include/
cp -u ~/dev/egre_mud/src/* deps/egre_mud/src/
cp -u ~/dev/egre_mud/include/* deps/egre_mud/include/

# Compile all of the dependencies in case we're making changes here
# instead of in their own projects
make FULL=1 all deps | tee compile_out

# Build the test suite and include the bash completions parse transform
erlc -o test/ -pa test/ -I include/ -I deps/ test/mud_test_completions.erl  test/mud_SUITE.erl
