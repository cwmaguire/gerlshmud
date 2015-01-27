-module(erlmud_player).

-export([procs/1]).
-export([create/1]).
%-export([add/3]).

-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), ?PV(K, PL, undefined)).

procs(Props) ->
    Fields = [room, items],
    lists:flatten([?PV(Field, Props) || Field <- Fields]).

create(Props) ->
    [{items, ?PV(room, Props)},
     {room, ?PV(items, Props)},
     {messages, ?PV(messages, Props)}].

%add(Props, item, Pid) ->

    %lists:keyreplace(item, 1, Props, 
