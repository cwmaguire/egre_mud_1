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

# drop log table
# See ~/.pgpass for password
psql -h localhost -U egre -c 'drop table log cascade; drop table pid_id cascade;'

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
  ./log_to_json
else
  echo "No logs/egre.log file found"
fi

# create db view
psql -h localhost -U egre <<EOF
create view log_view as
select
 log.tag,
 log.pid "pid",
 left(p1.id, 10) "id",
 event_type,
 stage,
 subscribe "sub?",
 rules_module,
 event_source,
 left(p2.id, 10) "src_id",
 event_target,
 left(p3.id, 10) "tgt_id",
 message,
 owner,
 left(p4.id, 10) "own_id",
 "character" "char",
 left(p5.id, 10) "char_id"
 from log
 left join pid_id p1 on log.pid = p1.pid and log.tag = p1.tag
 left join pid_id p2 on event_source = p2.pid and log.tag = p2.tag
 left join pid_id p3 on event_target = p3.pid and log.tag = p3.tag
 left join pid_id p4 on owner = p4.pid and log.tag = p4.tag
 left join pid_id p5 on "character" = p5.pid and log.tag = p5.tag;
EOF

psql -h localhost -U egre <<EOF
create view log_view_short as
SELECT
    to_char(log.timestamp, 'SS.US') "TS",
    log.pid,
    left(p1.id, 10) AS id,
    log.event_type,
    log.stage,
    log.subscribe AS "sub?",
    log.rules_module,
    log.message
FROM log
     LEFT JOIN pid_id p1 ON log.pid = p1.pid AND log.tag = p1.tag
     LEFT JOIN pid_id p2 ON log.event_source = p2.pid AND log.tag = p2.tag
     LEFT JOIN pid_id p3 ON log.event_target = p3.pid AND log.tag = p3.tag
     LEFT JOIN pid_id p4 ON log.owner = p4.pid AND log.tag = p4.tag
     LEFT JOIN pid_id p5 ON log."character" = p5.pid AND log.tag = p5.tag;
EOF
