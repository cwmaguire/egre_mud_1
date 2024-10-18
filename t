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

# Everything after -erl_args is ignored by CT
CT_OPTS="-config test/test.config ${CASES} -erl_args -config rel/sys.config " \
  EGRE_LOG_PATH=$(pwd)/logs \
  make ct | tee  out

if [[ -e logs/egre.log ]] ; then
  cat log_wrappers/json_head <(sed -e 's/.*/&,/' logs/egre.log) log_wrappers/json_tail > logs/egre_mud_1_log.js
  cp log_wrappers/*.{js,css,html} logs/
else
  echo "No logs/egre.log file found"
fi
