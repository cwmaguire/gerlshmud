%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_attack_melee).
-behaviour(gerlshmud_attack).

-export([should_attack/1]).

should_attack(Props) ->
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " is not wielded">>,
    is_wielded(Props) andalso is_attack(Props) orelse {false, Message}.

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.

is_attack(Props) ->
    true == proplists:get_value(is_attack, Props, false).
