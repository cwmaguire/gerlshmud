%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud).

-export([create/2]).

create(Type, Props) ->
    gerlshmud_object:start_link('TODO_add_object_id', Type, Type:create(Props)).
