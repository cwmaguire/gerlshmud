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
-module(erlmud_handler_vector_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

%% Attacking
attempt({#parents{character = Character},
         Props,
         {Attacker, attack, Target}})
  when Attacker == Character ->
    Log = [{?SOURCE, Attacker},
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
        false ->
            {{fail, <<"Vector is not activated">>}, false, Props, Log}
    end;

attempt({#parents{},
         Props,
         {allocate, Required, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, Type},
           {?TARGET, Self}],
    {succeed, true, Props, Log};

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

%% Defending
%% I don't think we need to know when someone attacks our character,
%% we'll automatically get events for calc-hit and calc-damage

%% Defend
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Character},
           {vector, AttackVector}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props, Log};
                Amount ->
                    {succeed,
                     {Attacker, calc, Hit - Amount, on, Character, with, AttackVector},
                     true,
                     Props,
                     Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Types, damage, Damage, to, Character, with, AttackVector}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_damage},
           {damage, Damage},
           {?TARGET, Character},
           {vector, AttackVector}],
    case should_defend(Props) of
        true ->
            case erlmud_modifiers(Props, defence, damage, Types) of
                0 ->
                    {succeed, false, Props, Log};
                Amount ->
                    {succeed,
                     {Attacker, calc, Damage - Amount, on, Character, with, AttackVector},
                     true,
                     Props,
                     Log}
            end;
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

succeed({Props, {Attacker, attack, Target}}) when is_pid(Target) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Target}],
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case {Character, IsAttacking} of
        {Attacker, false} ->
            erlmud_object:attempt(self(), {Attacker, attack, Target, with, self()});
        {Target, false} ->
            ok;
            %erlmud_object:attempt(Character, {Character, attack, Attacker});
        _ ->
            ok
    end,
    {Props, Log};

succeed({Props, {Attacker, counter_attack, Target}}) when is_pid(Target) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, counter_attack},
           {?TARGET, Target}],
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case {Character, IsAttacking} of
        {Attacker, false} ->
            erlmud_object:attempt(self(), {Attacker, attack, Target, with, self()});
        {Target, false} ->
            erlmud_object:attempt(Character, {Character, attack, Attacker});
        _ ->
            ok
    end,
    {Props, Log};

%% An attack by our character has been successfully instigated using this process:
%% we'll register for resources and implement the attack when we have them.
succeed({Props, {Character, attack, Target, with, Self}}) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {vector, Self}],
    reserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, Target}),
    Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true}),
    {Props3, Log};

succeed({Props, {allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?TARGET, Self}],
    Allocated = update_allocated(Amt, Type, Props),
    Required = proplists:get_value(resources, Props, []),
    HasResources = has_resources(Allocated, Required),
    RemainingAllocated =
        case HasResources of
            true ->
                attack(Props),
                deallocate(Allocated, Required);
            _ ->
                Allocated
        end,
    Props2 = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated}),
    {Props2, Log};

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    Damage = proplists:get_value(attack_damage_modifier, Props, 0),
    Log = [{?SOURCE, Character},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Target},
           {vector, Self}],
    erlmud_object:attempt(self(), {Character, calc, Types, damage, Damage, to, Target, with, Self}),
    {Props, Log};

succeed({Props, {Character, calc, Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, calc_hit},
           {?TARGET, Target},
           {hit, Miss}],
    % TODO: say "you missed!"
    {Props, Log};

succeed({Props, {Character, calc, Types, damage, Damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    log([{?EVENT, calc_damage},
         {object, Self},
         {props, Props},
         {character, Character},
         {damage, Damage},
         {?TARGET, Target},
         {damage_types, Types},
         {result, succeed}]),
    erlmud_object:attempt(self(), {Character, does, Damage, to, Target, with, Self}),
    Props;

succeed({Props, {Character, calc, Types, NoDamage, to, Target, with, Self}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    Log = [{?SOURCE, Character},
           {?EVENT, calc_damage},
           {damage, NoDamage},
           {damage_types, Types},
           {?TARGET, Target},
           {vector, Self}],
    {Props, Log};

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

attack(Props) ->
    Character = proplists:get_value(character, Props),
    Target = proplists:get_value(target, Props),
    Types = proplists:get_value(attack_types, Props, 0),
    erlmud_object:attempt(self(), {Character, calc, Types, success, Success, on, Target, with, self()}).

calc_base_success(Props) ->
    SucessBase = proplists:get_value(attack_success_base, Props, 0),
    SuccessModifier = proplists:get_value(attack_success_modifier, Props, 0),
    random:seed(


should_attack(Props) ->
    is_wielded(Props) andalso is_attack(Props).

should_defend(Props) ->
    is_wielded(Props) andalso is_defence(Props).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.

is_attack(Props) ->
    true == proplists:get_value(is_attack, Props, false).

is_defence(Props) ->
    true == proplists:get_value(is_defence, Props, false).

unreserve(Character, Props) when is_list(Props) ->
    [unreserve(Character, Resource) || {Resource, _Amt} <- proplists:get_value(resources, Props, [])];
unreserve(Character, Resource) ->
    erlmud_object:attempt(self(), {Character, unreserve, Resource, for, self()}).

reserve(Character, Props) when is_list(Props) ->
    [reserve(Character, Resource, Amount) || {Resource, Amount} <- proplists:get_value(resources, Props, [])].

reserve(Character, Resource, Amount) ->
    erlmud_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, self()}).

update_allocated(New, Type, Props) ->
    Allocated = proplists:get_value(allocated_resources, Props, #{}),
    Curr = maps:get(Type, Allocated, 0),
    Allocated#{Type => Curr + New}.

deallocate(Allocated, Required) ->
    lists:foldl(fun subtract_required/2, Allocated, Required).

subtract_required({Type, Required}, Allocated) ->
    #{Type := Amt} = Allocated,
    Allocated#{Type := min(0, Amt - Required)}.

has_resources(Allocated, Required) ->
    {_, AllocApplied} = lists:foldl(fun apply_resource/2, {Allocated, []}, Required),
    case lists:filter(fun is_resource_lacking/1, AllocApplied) of
        [] ->
            true;
        _ ->
            false
    end.

apply_resource(_Resource = {Type, Required},
               {Allocated, Applied0}) ->
    AllocAmt = maps:get(Type, Allocated, 0),
    Applied1 = [{Type, Required - AllocAmt} | Applied0],
    {Allocated#{Type => 0}, Applied1}.

is_resource_lacking({_Type, Amount}) when Amount =< 0 ->
    false;
is_resource_lacking(_) ->
    true.

log(Props) ->
    erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).
