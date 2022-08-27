%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_defence_armor).
-behaviour(gerlshmud_defence).

-export([should_defend/1]).

-include("include/gerlshmud.hrl").

should_defend(Props) ->
    is_wielded(Props) andalso is_defence(Props).

is_defence(Props) ->
    true == proplists:get_value(is_defence, Props, false).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.

