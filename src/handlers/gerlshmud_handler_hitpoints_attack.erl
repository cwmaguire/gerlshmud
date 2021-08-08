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
-module(gerlshmud_handler_hitpoints_attack).

-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{owner = Owner},
         Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Efffect}}) ->
    Log = [{?EVENT, Effect},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           {Effect, Amount}],
    case is_hp_effect(Effect) of
        true ->
            {succeed, true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props,
         {Attacker, cause, Amount, 'of', Effect, to, Owner, with, _Effect}}) ->
    Log = [{?EVENT, Effect},
           {?TARGET, Owner},
           {from, Attacker},
           {Effect, Amount}],
    {Props2, Log2} = take_damage(Attacker, Owner, Amount, Effect, Props),
    {Props2, Log2 ++ Log};

succeed({Props, _Msg}) ->
    Props.

fail({Props, _Message, _Reason}) ->
    Props.

take_damage(Attacker, Owner, Amount, EffectType, Props) ->
    Owner = proplists:get_value(owner, Props),
    Hp = proplists:get_value(hitpoints, Props, 0) - Amount,
    Log = [{hp, Hp}],


    case Hp of
        X when X < 1 ->
            Owner = proplists:get_value(owner, Props),
            ct:pal("Killing ~p~n", [Owner]),
            gerlshmud_object:attempt(Owner, {Attacker, killed, Owner, with, EffectType});
        _ ->
            ok
    end,
    Props2 = lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}),
    {Props2, Log}.

is_hp_effect(blunt_force) ->
    true;
is_hp_effect(_) ->
    false.

