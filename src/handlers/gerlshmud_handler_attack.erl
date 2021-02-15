%% Copyright (c) 2016, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_handler_attack).
-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATTEMPT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Attacking
attempt({#parents{character = Character},
         Props,
         {Character, attack, Target}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character},
         Props,
         {Character, counter_attack, Target}}) ->
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {is_attacking, IsAttacking}],
    case IsAttacking of
        false ->
            {succeed, true, Props, Log};
        _ ->
            %% If _other_ vectors aren't yet attacking the Target then they'll join in.
            %% I'm not sure how that would happen unless the player can set what they're
            %% attacking with for each individual attack. In that case they'll need to
            %% set what their default counterattack is.
            {succeed, false, Props, Log}
    end;

attempt({#parents{character = Character},
         Props,
         {Character, attack, Target, with, Self}})
  when Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target},
           {vector, Self}],
    case should_attack(Props) of
        true ->
            {succeed, true, Props, Log};
        {false, Message} ->
            {{fail, Message}, false, Props, Log}
    end;

attempt({#parents{},
         Props,
         {Attacker, killed, Target, with, AttackVector}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, killed},
           {?SOURCE, Target},
           {vector, AttackVector}],
    case proplists:get_value(target, Props) of
        Target ->
            Log2 = [{?TARGET, Target} | Log],
            {succeed, true, Props, Log2};
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({#parents{character = Character}, Props, {Character, stop_attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character},
         Props,
         {die, Character}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUCCEED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeed({Props, {Attacker, killed, Target, with, AttackVector}}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Attacker},
           {?TARGET, Target},
           {vector, AttackVector}],
    Character = proplists:get_value(character, Props),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {?TARGET, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Attacker, Attack, Target}})
  when is_pid(Target),
       Attack = attack; Attack == counter_attack ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Target}],
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case IsAttacking of
        false ->
            gerlshmud_object:attempt(self(), {Character, attack, Target, with, self()});
        _ ->
            ok
    end,
    {Props, Log};

succeed({Props, {Character, attack, Target, with, Self}}) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {vector, Self}],

    Props2 = lists:keystore(target, 1, Props, {target, Target}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true}),
    {Props3, Log};

succeed({Props, {Character, stop_attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, stop_attack}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, {Character, die}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, die}],
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false}),
    {Props3, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

should_attack(Props) ->
    ShouldAttackModule = proplists:get_value(should_attack_module, Props),
    ShouldAttackModule:should_attack(Props).

unreserve(Character, Props) when is_list(Props) ->
    [unreserve(Character, Resource) || {Resource, _Amt} <- proplists:get_value(resources, Props, [])];
unreserve(Character, Resource) ->
    gerlshmud_object:attempt(self(), {Character, unreserve, Resource, for, self()}).


log(Props) ->
    gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).
