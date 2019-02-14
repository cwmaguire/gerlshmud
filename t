#!/usr/bin/env bash
rm -rf logs/*
mkdir logs/images/
cp log_wrappers/images/*.png logs/images/

CT_OPTS="-config test/test.config -erl_args -config rel/sys.config" \
  GERLSHMUD_LOG_PATH=$(pwd)/logs \
  make ct | tee  out
cat log_wrappers/json_head <(sed -e 's/.*/&,/' logs/gerlshmud.log) log_wrappers/json_tail > logs/gerlshmud_log.js
cp log_wrappers/*.{js,css,html} logs/
