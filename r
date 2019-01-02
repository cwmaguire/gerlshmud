export ERLMUD_LOG_PATH=/Users/cmaguire/dev/erlmud/logs/
gtruncate -s 0 logs/*.log && make clean && make rel && _rel/erlmud/bin/erlmud console
