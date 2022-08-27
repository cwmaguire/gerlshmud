%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_attack).
-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{}, Props, {Attacker, attack, Self}}) when Self == self() ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, attack},
           {?TARGET, Self}],
    {succeed, true, Props, Log};
attempt({#parents{}, Props, {Self, attack, Target, with, AttackType}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, attack},
           {?TARGET, Target},
           {attack_type, AttackType}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Self, attack, Target, with, AttackType}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, attack},
           {?TARGET, Target},
           {vector, AttackType}],
    Props2 = lists:keystore(is_attacking, 1, Props, {is_attacking, true}),
    {Props2, Log};
succeed({Props, {Attacker, attack, Self}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, attack},
           {?TARGET, Self}],
    case proplists:get_value(is_attacking, Props) of
        true ->
            ok;
        _ ->
            gerlshmud_object:attempt(self(), {self(), counter_attack, Attacker})
    end,
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
