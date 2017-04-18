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
attempt({#parents{owner = Owner}, Props, Msg = {_Source, killed, Owner, with, _AttackVector}}) ->
    log([<<"attempt: ">>, Msg, <<", props: ">>, Props]),
    {succeed, _Subscribe = true, Props};

%% We have died
attempt({#parents{owner = Owner}, Props, _Msg = {Owner, die}}) ->
    {succeed, _Subscribe = true, Props};

%% Something is attack us and we are dead
attempt({#parents{owner = Owner}, Props, _Msg = {_Attacker, calc, _Hit, on, Owner, with, _AttackVector}}) ->
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{owner = Owner}, Props, _Msg = {_Attacker, calc, _Damager, to, Owner, with, _AttackVector}}) ->
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;

%% TODO fail everything when dead, e.g. move, wield, attack, etc.


attempt(_) ->
    undefined.

succeed({Props, {_Source, killed, Owner, with, _AttackVector}}) ->
    %log("Character ~p killed by ~p, sending die: ~p~nprops: ~p~n",
        %[Owner, Source, Owner, Props]),
    erlmud_object:attempt(self(), {die, Owner}),
    Props;

succeed({Props, {Owner, die}}) ->
    CorpseCleanupMilis = application:get_env(erlmud, corpse_cleanup_milis, 10 * 60 * 1000),
    erlmud_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Owner}),
    lists:keystore(is_alive, 1, Props, {is_alive, false});

succeed({Props, _Msg}) ->
    %log("saw ~p succeed with props ~p~n", [Msg, Props]),
    throw(should_never_happen),
    Props.

fail({Props, _Message, _Reason}) ->
    %log("saw ~p fail with props ~p~n", [Message, Props]),
    throw(should_never_happen),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
