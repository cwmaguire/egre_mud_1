#!/usr/bin/env bash

# re-copy the logs and log wrapper code into the logs/ directory.
# Normally we do this in ./t when we run tests, but when we're working on
# logging HTML, JS and CSS, it's annoying to have to run the tests.

cat log_wrappers/json_head <(sed -e 's/.*/&,/' logs/egre.log) log_wrappers/json_tail > logs/egre_mud_1_log.js \
  && cp log_wrappers/*.{js,css,html} logs/
