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

remove(Type, Obj, Props) ->
    Objs = proplists:get_value(Type, Props, []),
    lists:keystore(Type, 1, lists:delete(Obj, Objs)).

handle(Props, {attempt, {move, Self, Direction}}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
handle(Props, {succeed, {move, Self, Source, Target}})
    when Self == self(), is_pid(Target) ->
    io:format("Player ~p: moved from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, player, self()}),
    gen_server:cast(Target, {add, player, self()}),
    set(room, Target, Props);
handle(Props, {succeed, {move, Self, Source, Direction}})
    when Self == self(), is_atom(Direction) ->
    io:format("Player ~p failed to move ~p from ~p~n", [self(), Direction, Source]),
    Props;
handle(Props, Msg = {attempt, _}) ->
    log(Msg, Props),
    {succeed, true, Props};
handle(Props, Msg) ->
    log(Msg, Props),
    Props.

log(Msg, Props) ->
    io:format("Player received: ~p~n"
              "with props: ~p~n",
              [Msg, Props]).
