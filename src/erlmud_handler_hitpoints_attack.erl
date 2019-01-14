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
-module(erlmud_handler_hitpoints_attack).

-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({#parents{owner = Owner},
         Props,
         {Character, does, Damage, to, Owner, with, AttackVector}}) ->
    Log = [{type, damage},
           {source, Character},
           {target, Owner},
           {damage, Damage},
           {vector, AttackVector}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _Msg = {Attacker, does, Damage, to, Owner, with, AttackVector}}) ->
    Log = [{type, damage},
           {target, Owner},
           {from, Attacker},
           {damage, Damage},
           {vector, AttackVector}],
    {Props2, Log2} = take_damage(Attacker, Owner, Damage, AttackVector, Props),
    {Props2, Log2 ++ Log};
succeed({Props, _Msg}) ->
    Props.

fail({Props, _Message, _Reason}) ->
    Props.

take_damage(Attacker, Owner, Damage, AttackVector, Props) ->
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    Log = [{hp, Hp}],
    case Hp of
        X when X < 1 ->
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {Attacker, killed, Owner, with, AttackVector});
        _ ->
            ok
    end,
    Props2 = lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}),
    {Props2, Log}.
