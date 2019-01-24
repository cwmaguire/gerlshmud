#!/usr/bin/env bash
rm -rf logs/*
mkdir logs/images/
cp log_wrappers/images/*.png logs/images/

CT_OPTS="-config test/test.config" ERLMUD_LOG_PATH=$(pwd)/logs make ct | tee  out
cat log_wrappers/json_head <(sed -e 's/.*/&,/' logs/erlmud.log) log_wrappers/json_tail > logs/erlmud_log.js
cp log_wrappers/*.{js,css,html} logs/
