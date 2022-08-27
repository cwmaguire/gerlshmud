%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_spell_inject_self).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{character = Character},
         Props,
         {Character, memorize, SpellName}})
  when is_binary(SpellName) ->
    case is_name(Props, SpellName) of
        true ->
            NewMessage = {Character, memorize, self()},
            Log = [{?EVENT, inject_self},
                   {action, memorize},
                   {name, SpellName}],
            Result = {resend, Character, NewMessage},
            {Result, _Subscribe = false, Props, Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    SpellName = proplists:get_value(name, Props, ""),
    match == re:run(SpellName, Name, [{capture, none}]).
