%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_counterattack).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{}, Props, {Attacker, attack, Self}}) when Self == self() ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Self}],
    {succeed, true, Props, Log};

attempt({#parents{}, Props, {Self, attack, Target}}) when Self == self() ->
    Log = [{?EVENT, attack},
           {?SOURCE, Self},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt(_) ->
    undefined.

succeed({Props, {Character, stop_attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    Props = lists:keystore(is_attacking, 1, Props, {is_attacking, false}),
    {Props, Log};

succeed({Props, {Attacker, attack, Target}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, attack},
           {?TARGET, Target}],
    case proplists:get_value(owner, Props) of
        Attacker ->
            % TODO why would I need this here when it's handled in g_h_attack?
            Props2 = lists:keystore(is_attacking, 1, Props, {is_attacking, true}),
            {Props2, Log};
        Target ->
            case proplists:get_value(is_attacking, Props) of
                true ->
                    ok;
                _ ->
                    gerlshmud_object:attempt(self(), {self(), counter_attack, Attacker})
            end,
            {Props, Log};
        _ ->
            {Props, Log}
    end;

succeed({Props, {Self, counter_attack, Target}}) ->
    Log = [{?SOURCE, Self},
           {?EVENT, counter_attack},
           {?TARGET, Target}],
    gerlshmud_object:attempt(self(), {self(), attack, Target}),
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _Result, _Msg}) ->
    Log = [{?EVENT, attack},
           {?TARGET, self()}],
    {Props, Log}.
