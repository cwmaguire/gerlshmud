-module(erlmud_player).

-export([handle/2]).
-export([add/3]).
-export([remove/3]).

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {room, Obj}).

add(Type, Obj, Props) ->
    OldProp = proplists:get_value(Type, Props, []),
    NewProp = {Type, [Obj | OldProp]},
    lists:keystore(Type, 1, Props, NewProp).

%room(Exit, Props) when is_atom(Exit) ->
    %room(proplists:get_value(room, Props), Exit).

%room(undefined, _) ->
    %undefined;
%room(Room, Exit) ->

remove(Type, Obj, Props) ->
    Objs = proplists:get_value(Type, Props, []),
    lists:keystore(Type, 1, lists:delete(Obj, Objs)).

handle(Props, Msg = {attempt, _}) ->
    log(Msg, Props),
    {succeed, true, Props};
handle(Props, {succeed, {move, Self, Source, Target}}) when Self == self() ->
    io:format("Player ~p: moved from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, player, self()}),
    gen_server:cast(Target, {add, player, self()}),
    set(room, Target, Props);
handle(Props, Msg) ->
    log(Msg, Props),
    Props.

log(Msg, Props) ->
    io:format("Player received: ~p~n"
              "with props: ~p~n",
              [Msg, Props]).
