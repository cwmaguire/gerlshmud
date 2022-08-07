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
-module(gerlshmud_handler_test_connection_attack).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

is_dead_action(revive) ->
    true;
is_dead_action(_) ->
    false.

%% TODO I think this can be removed. A lot of this attack stuff looks pretty old.
%% It is part of the test connection handlers though, so I'll have to dig into it.

attempt({#parents{owner = Owner},
         Props,
         _Msg = {killed, Attack, Source, Owner}}) ->
    % This is just a guess, per the message above I don't know if this is used
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner},
           {vector, Attack}],
    {succeed, _Subscribe = true, Props, Log};
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Owner, die}}) ->
    Log = [{?EVENT, die},
           {?SOURCE, Owner}],
    {succeed, _Subscribe = true, Props, Log};
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Action, Attack, Owner, Target, _}})
    when Action == calc_hit; Action == calc_damage ->
    Log = [{?EVENT, Action},
           {?SOURCE, Owner},
           {?TARGET, Target},
           {vector, Attack}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p ~p when ~p is dead~n", [Owner, Action, Target, Owner]),
            {{fail, target_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Action, Attack, Attacker, Owner, _}})
    when Action == calc_hit; Action == calc_damage ->
    Log = [{?EVENT, Action},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p ~p when ~p is dead~n", [Attacker, Action, Owner, Owner]),
            {{fail, target_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {calc_next_attack_wait, Attack, Attacker, Owner, _, _}}) ->
    Log = [{?EVENT, calc_next_attack_wait},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p to attack ~p when ~p is dead~n", [Attacker, calc_next_attack_wait, Owner, Owner]),
            {{fail, target_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {calc_next_attack_wait, Attack, Owner, Target, _, _}}) ->
    Log = [{?EVENT, calc_next_attack_wait},
           {vector, Attack},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot ~p to attack ~p when ~p is dead~n", [Owner, calc_next_attack_wait, Target, Target]),
            {{fail, target_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {attack, Attack, Attacker, Owner}}) ->
    Log = [{?EVENT, attack},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            %log("~p cannot attack ~p when ~p is dead~n", [Attacker, Owner, Owner]),
            {{fail, target_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {attack, Attack, Owner, Target}}) ->
    Log = [{?EVENT, attack},
           {vector, Attack},
           {?SOURCE, Owner},
           {?TARGET, Target}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, attacker_is_dead}, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         Msg}) when Owner == element(2, Msg) ->
    Action = element(1, Msg),
    Log = [{?EVENT, Action}],
    IsAlive = proplists:get_value(is_alive, Props, false),
    IsDeadAction = is_dead_action(Action),
    case IsAlive orelse IsDeadAction of
        true ->
            {succeed, _Subscribe = false, Props, Log};
        false ->
            AliveOrDead = case IsAlive of true -> "alive"; false -> "dead" end,
            FailMsg = iolist_to_binary(io_lib:format("~p cannot ~p when ~p~n",
                                                  [Owner, Action, AliveOrDead])),
            {{fail, FailMsg}, _Subscribe = false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         {calc_hit, Attack, Attacker, Owner, _}}) ->
    Log = [{?EVENT, calc_hit},
           {vector, Attack},
           {?SOURCE, Attacker},
           {?TARGET, Owner}],
    case proplists:get_value(is_alive, Props) of
        false ->
            {{resend, Attacker, {killed, Attack, Attacker, Owner}}, false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

%% This no longer seems to match any generated messages
%% See protocol.csv
succeed({Props, {killed, Attack, Source, Owner}}) ->
    Log = [{?EVENT, killed},
           {vector, Attack},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    gerlshmud_object:attempt(self(), {Owner, die}),
    {Props, Log};
%% Why is test_connection_attack kicking off the cleanup?
%% _life_attack kicks it off too
succeed({Props, {Target, die}}) ->
    Log = [{?EVENT, die},
           {?SOURCE, Target}],
    Owner = proplists:get_value(owner, Props),
    Props2
    = case Target of
        X when X == Owner ->
            CorpseCleanupMilis = application:get_env(gerlshmud, corpse_cleanup_milis, 10 * 60 * 1000),
            gerlshmud_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Owner}),
            lists:keystore(is_alive, 1, Props, {is_alive, false});
        _ ->
            Props
    end,
    {Props2, Log};
succeed({Props, _Msg}) ->
    throw(should_never_happen),
    Props.

fail({Props, _Message, _Reason}) ->
    throw(should_never_happen),
    Props.
