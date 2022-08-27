%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_attack_spell).
-behaviour(gerlshmud_attack).

-export([should_attack/1]).

should_attack(Props) ->
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " is not memorized">>,
    IsMemorized = proplists:get_value(is_memorized, Props),
    IsAttack = proplists:get_value(is_attack, Props),
    IsMemorized andalso IsAttack orelse {false, Message}.
