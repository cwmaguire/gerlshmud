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
         {_Character, does, _Damage, to, Owner, with, _AttackVector}}) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, Msg = {Attacker, does, Damage, to, Owner, with, AttackVector}}) ->
    log([<<"saw ">>, Msg, <<"succeed">>]),
    take_damage(Attacker, Owner, Damage, AttackVector, Props);
succeed({Props, _Msg}) ->
    Props.

fail({Props, Message, _Reason}) ->
    log([<<"saw ">>, Message, <<" fail with props ">>, Props]),
    Props.

take_damage(Attacker, Owner, Damage, AttackVector, Props) ->
    log([<<"taking ">>, Damage, <<" points of damage.">>]),
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            log([<<"dying; hp = ">>, Hp]),
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {Attacker, killed, Owner, with, AttackVector});
        _ ->
            log([<<"Not dying; hitpoints = ">>, Hp]),
            ok
    end,
    lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}).

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
