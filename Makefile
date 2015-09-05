PROJECT = erlmud
DEPS = cowboy jsx lists
COMPILE_FIRST = erlmud_object

dep_lists = git https://github.com/cwmaguire/lists master

include erlang.mk
