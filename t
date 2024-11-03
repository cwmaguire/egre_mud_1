#!/usr/bin/env bash
rm -rf logs/*
mkdir logs/images/
cp log_wrappers/images/*.png logs/images/

# run only cases specific on the command line, if any
if [ -n $1 ]
then
  CASES="-case $*"
  echo "Running with ${CASES}"
else
  echo "Running all cases"
  exit
fi

# pull in the latest files from the deps so I don't have to worry
# about patching changes back to the original repos when I make
# changes to the deps/egre{,_mud}/src files. This way I can edit
# ~/dev/egre{,_mud}/src/ files directly.
# (-u means only copy newer files)
cp -u ~/dev/egre/src/* deps/egre/src/
cp -u ~/dev/egre/include/* deps/egre/include/
cp -u ~/dev/egre_mud/src/* deps/egre_mud/src/
cp -u ~/dev/egre_mud/include/* deps/egre_mud/include/

# Make sure we have the latest test suite compiled
touch test/mud_SUITE.erl

# Compile all of the dependencies in case we're making changes here
# instead of in their own projects
make FULL=1 all deps

# Everything after -erl_args is ignored by CT
CT_OPTS="-config test/test.config ${CASES} -erl_args -config rel/sys.config " \
  EGRE_LOG_PATH=$(pwd)/logs \
  make ct | tee  out

if [[ -e logs/egre.log ]] ; then
  cat log_wrappers/json_head <(sed -e 's/.*/&,/' logs/egre.log) log_wrappers/json_tail > logs/egre_mud_1_log.js
  cp log_wrappers/*.{js,css,html} logs/

  # create jq-compatible json
  log_to_json
else
  echo "No logs/egre.log file found"
fi
