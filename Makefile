PROJECT = erlmud
DEPS = cowboy jsx lists lager
COMPILE_FIRST = erlmud_object erlmud_handler

dep_lists = git https://github.com/cwmaguire/lists master

## copied from erlang.mk and added +native.
## Brings counterattack_behaviour test down from 1+ seconds
## to ~400ms.
## Also added lager parse transform
ERLC_OPTS = -Werror \
						+debug_info \
						+warn_export_vars \
						+warn_shadow_vars \
						+warn_obsolete_guard \
						+'{parse_transform, lager_transform}'
						#+native ##\

## copied from erlang.mk and added lager parse transform
TEST_ERLC_OPTS = +debug_info \
								 +warn_export_vars \
                 +warn_shadow_vars \
								 +warn_obsolete_guard \
								 +'{parse_transform, lager_transform}'

include erlang.mk
