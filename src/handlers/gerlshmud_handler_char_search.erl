%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_search).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{}, Props, {Player, search, Self}}) when Self == self() ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self}],
    Name = proplists:get_value(name, Props, "MissingName"),
    NewMessage = {Player, search, Self, named, Name, with, body_parts, []},
    Result = {resend, Self, NewMessage},
    {Result, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _Msg}) ->
    Props.

%% TODO
%% Implement search detection: i.e. a character detects a player searching them
%% (and eventually other NPC characters searching them)
fail({Props,
      character_detected_search,
      {Player, search, Self, named, _Name, with, body_parts, _BodyParts}}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self},
           {reason, character_detected_search}],
    counter_attack(Player, Self, Props),
    {Props, Log};
fail({Props, _Result, {Player, search, Self}}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self}],
    {Props, Log};
fail({Props, _Result, _Msg}) ->
    Log = [{?EVENT, search},
           {?TARGET, self()}],
    {Props, Log}.

counter_attack(Player, Self, Props) ->
    case proplists:get_value(is_attacking, Props) of
        true ->
            ok;
        _ ->
            gerlshmud_object:attempt(Self, {Self, attack, Player})
    end.
