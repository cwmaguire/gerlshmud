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
    log([{type, damage},
         {target, self()},
         {owner, Owner},
         {from, Character},
         {to, Owner},
         {damage, Damage},
         {vector, AttackVector}]),
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, _Msg = {Attacker, does, Damage, to, Owner, with, AttackVector}}) ->
    log([{stage, succeed},
         {type, damage},
         {target, self()},
         {owner, Owner},
         {from, Attacker},
         {to, Owner},
         {damage, Damage},
         {vector, AttackVector},
         {result, succeed}]),
    take_damage(Attacker, Owner, Damage, AttackVector, Props);
succeed({Props, _Msg}) ->
    Props.

fail({Props, Message, Reason}) ->
    log([{type, damage},
         {object, self()},
         {message, Message},
         {result, fail},
         {reason, Reason},
         {props, Props}]),
    Props.

take_damage(Attacker, Owner, Damage, AttackVector, Props) ->
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            log([{type, dying},
                 {hp, Hp},
                 {object, self()},
                 {owner, Owner},
                 {damage, Damage},
                 {props, Props}]),
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {Attacker, killed, Owner, with, AttackVector});
        _ ->
            log([{type, not_dying},
                 {hp, Hp},
                 {object, self()},
                 {owner, Owner},
                 {damage, Damage},
                 {props, Props}]),
            ok
    end,
    lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}).

log(Terms) ->
    erlmud_event_log:log(debug, [{module, ?MODULE} | Terms]).
