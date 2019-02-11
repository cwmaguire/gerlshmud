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
-module(erlmud_handler_life_attack).

-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

%% We have been killed
attempt({#parents{owner = Owner}, Props, Msg = {Source, killed, Owner, with, _AttackVector}}) ->
    log([<<"attempt: ">>, Msg, <<", props: ">>, Props]),
                log([{stage, attempt},
                     {?EVENT, killed},
                     {object, self()},
                     {props, Props},
                     {?SOURCE, Source},
                     {?TARGET, Owner},
                     {message, Msg},
                     {sub, true}]),
    {succeed, _Subscribe = true, Props};

%% We have died
attempt({#parents{owner = Owner}, Props, Msg = {Owner, die}}) ->
    log([{stage, attempt},
         {?EVENT, die},
         {object, self()},
         {props, Props},
         {?TARGET, Owner},
         {message, Msg},
         {sub, true}]),
    {succeed, _Subscribe = true, Props};

%% Something is attack us and we are dead
attempt({#parents{owner = Owner}, Props, Msg = {Attacker, calc, Hit, on, Owner, with, AttackVector}}) ->
    log([{stage, attempt},
         {?EVENT, calc_hit},
         {object, self()},
         {props, Props},
         {?SOURCE, Attacker},
         {?TARGET, Owner},
         {hit, Hit},
         {attack_vector, AttackVector},
         {message, Msg},
         {sub, false}]),
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{owner = Owner},
         Props,
         Msg = {Attacker, calc, Types, damage, Damage, to, Owner, with, AttackVector}}) ->
    log([{stage, attempt},
         {?EVENT, calc_damage},
         {object, self()},
         {props, Props},
         {?SOURCE, Attacker},
         {?TARGET, Owner},
         {damage, Damage},
         {attack_vector, AttackVector},
         {types, Types},
         {message, Msg},
         {sub, false}]),
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;

%% TODO fail everything when dead, e.g. move, wield, attack, etc.

attempt(_) ->
    undefined.

succeed({Props, {Source, killed, Owner, with, AttackVector}}) ->
    log([{stage, succeed},
         {?EVENT, killed},
         {object, self()},
         {props, Props},
         {?SOURCE, Source},
         {?TARGET, Owner},
         {handler, ?MODULE},
         {attack_vector, AttackVector}]),
    erlmud_object:attempt(self(), {Owner, die}),
    Props;

succeed({Props, {Owner, die}}) ->
    log([{stage, succeed},
         {?EVENT, die},
         {object, self()},
         {props, Props},
         {?TARGET, Owner}]),
    CorpseCleanupMilis = application:get_env(erlmud, corpse_cleanup_milis, 10 * 60 * 1000),
    erlmud_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Owner}),
    lists:keystore(is_alive, 1, Props, {is_alive, false});

succeed({Props, _Msg}) ->
    throw(should_never_happen),
    Props.

fail({Props, _Message, _Reason}) ->
    throw(should_never_happen),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
