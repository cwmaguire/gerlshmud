-module(erlmud).

-export([create/2]).

create(Type, Props) ->
    erlmud_object:start_link(Type, Type:create(Props)).
