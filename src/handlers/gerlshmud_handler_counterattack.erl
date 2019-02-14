%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(gerlshmud_handler_counterattack).
-behaviour(gerlshmud_handler).

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

attempt({#parents{}, _, _}) ->
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
            Props = lists:keystore(is_attacking, 1, Props, {is_attacking, true}),
            {Props, Log};
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
