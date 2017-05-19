#!/usr/bin/env bash
rm -rf logs/*cmaguire*
rm -rf logs/*lulu*
rm -rf logs/ct_run.*
rm -rf logs/erlmud.log
rm -rf logs/log.html
ERLMUD_LOG_PATH=$(pwd)/logs make ct | tee  out
cat logs/log_head logs/log.html logs/log_tail > logs/erlmud_log.html
