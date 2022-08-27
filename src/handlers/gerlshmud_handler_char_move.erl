%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_move).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Self, move, Direction}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {direction, Direction}],
    case proplists:get_value(owner, Props) of
        undefined ->
            {{fail, <<"Character doesn't have room">>}, false, Props, Log};
        Room ->
            Log2 = [{from, Room} | Log],
            {{resend, Self, {Self, move, Direction, from, Room}}, false, Props, Log2}
    end;
attempt({#parents{}, Props, {Self, move, Dir, from, From}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {direction, Dir},
           {from, From}],
    {succeed, true, Props, Log};
attempt({#parents{}, Props, {Self, move, from, From, to, To, via, Exit}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Self, move, from, Source, to, Target, via, Exit}}) when Self == self() ->
    Log = [{?EVENT, move},
           {?SOURCE, Self},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    NewProps = set(room, Target, set(owner, Target, Props)),
    case proplists:get_value(is_attacking, Props) of
        true ->
            gerlshmud_object:attempt(self(), {self(), stop_attack});
        _ ->
            ok
    end,
    {NewProps, Log};
succeed({Props, {Self, move, Direction, from, Source}}) when Self == self(), is_atom(Direction) ->
    % gerlshmud_handler_exit_move should have turned this into:
    % {Self, move, from, Source, to, Target, via, Exit}
    Log = [{?EVENT, move},
           {?SOURCE, Self},
           {direction, Direction},
           {from, Source}],
    % TODO Let the player know they didn't get anywhere: "There is no exit <Direction> here."
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).
