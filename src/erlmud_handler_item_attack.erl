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
-module(erlmud_handler_item_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

%% Attacking
attempt({#parents{character = Character},
         Props,
         {Attacker, attack, _Target}})
  when Attacker == Character ->
    {succeed, true, Props};

attempt({#parents{character = Character},
         Props,
         {Character, counter_attack, Target}}) ->
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    log([{type, attack},
         {object, self()},
         {target, Target},
         {character, Target},
         {source, Character},
         {is_attacking, IsAttacking}]),
    case IsAttacking of
        false ->
            {succeed, true, Props};
        _ ->
            %% If _other_ items aren't yet attacking the Target then they'll join in.
            %% I'm not sure how that would happen unless the player can set what they're
            %% attacking with for each individual attack. In that case they'll need to
            %% set what their default counterattack is.
            {succeed, false, Props}
    end;

attempt({#parents{character = Character},
         Props,
         {Character, attack, _Target, with, Self}})
  when Self == self() ->
    case should_attack(Props) of
        true ->
            {succeed, true, Props};
        false ->
            {{fail, <<"Item is not wielded or is not activated">>}, false, Props}
    end;

attempt({#parents{},
         Props,
         {allocate, _Required, 'of', _Type, to, Self}})
  when Self == self() ->
    {succeed, true, Props};

attempt({#parents{},
         Props,
         {_Attacker, killed, Target, with, _AttackVector}}) ->
    case proplists:get_value(target, Props) of
        Target ->
            {succeed, true, Props};
        _ ->
            {succeed, false, Props}
    end;

%% Defending
%% I don't think we need to know when someone attacks our character,
%% we'll automatically get events for calc-hit and calc-damage

%% Defend
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector}}) ->
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Attacker, calc, Hit - Amount, on, Character, with, AttackVector},
                     true,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Damage, to, Character, with, AttackVector}}) ->
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Attacker, calc, Damage - Amount, on, Character, with, AttackVector},
                     true,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;

attempt({#parents{character = Character}, Props, {Character, stop_attack}}) ->
    {succeed, true, Props};

attempt({#parents{character = Character},
         Props,
         {die, Character}}) ->
    {succeed, true, Props};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {_Attacker, killed, Target, with, AttackVector}}) ->
    log([{type, killed},
         {object, self()},
         {props, Props},
         {target, Target},
         {attack_vector, AttackVector},
         {result, succeed}]),
    Character = proplists:get_value(character, Props),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false});

%% I feel like the character should kick off a counter-attack message when hit, but that means
%% on _every_ hit I'm sending out a counter-attack message that's going to get ignored.
%% If the _item_ sees the attack it can check if it's already engaged in a target.
%% However, if the player decides to change targets, then I'd need a different event to
%% differentiate from "Char attack Target (because it attacked us)" and "Char attack Target
%% (because the player said so)".
%%
%% I think I'll assume that event messages are free. If I start letting optimization creep
%% in (especially without measuring) this design is going to get gross fast.
%%
%% I'm probably going to need some counterattack logic at some point anyway.
%%
%% We only get one handler per event so if I need a counterattack handler I can give it a
%% distinct, but similar, event.

succeed({Props, {Attacker, attack, Target}}) when is_pid(Target) ->
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    case {Character, IsAttacking} of
        {Attacker, false} ->
            log([{type, attack},
                 {object, self()},
                 {character, Character},
                 {target, Target},
                 {is_attacking, IsAttacking},
                 {props, Props},
                 {result, succeed}]),
            erlmud_object:attempt(self(), {Attacker, attack, Target, with, self()});
        {Target, false} ->
            ok;
            %erlmud_object:attempt(Character, {Character, attack, Attacker});
        _ ->
            ok
    end,
    Props;

succeed({Props, {Attacker, counter_attack, Target}}) when is_pid(Target) ->
    Character = proplists:get_value(character, Props),
    IsAttacking = proplists:get_value(is_attacking, Props, false),
    log([{type, counter_attack},
         {object, self()},
         {target, Target},
         {character, Character},
         {props, Props},
         {result, succeed}]),
    case {Character, IsAttacking} of
        {Attacker, false} ->
            erlmud_object:attempt(self(), {Attacker, attack, Target, with, self()});
        {Target, false} ->
            erlmud_object:attempt(Character, {Character, attack, Attacker});
        _ ->
            ok
    end,
    Props;

%% An attack by our character has been successfully instigated using this process:
%% we'll register for resources and implement the attack when we have them.
succeed({Props, {Character, attack, Target, with, _Self}}) ->
    log([{type, attack},
         {object, self()},
         {props, Props},
         {character, Character},
         {target, Target},
         {result, succeed}]),
    reserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, Target}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true});

succeed({Props, {allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
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
    log([{type, allocate},
         {object, self()},
         {amount, Amt},
         {resource, Type},
         {allocated, Allocated},
         {required, Required},
         {remaining_allocated, RemainingAllocated},
         {has_resources, HasResources},
         {props, Props},
         {result, succeed}]),
    _Props = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated});

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    Damage = proplists:get_value(attack_damage_modifier, Props, 1),
    log([{type, calc_hit},
         {object, Self},
         {props, Props},
         {character, Character},
         {hit, Hit},
         {target, Target},
         {result, succeed}]),
    erlmud_object:attempt(self(), {Character, calc, Damage, to, Target, with, Self}),
    Props;

succeed({Props, {Character, calc, Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    log([{type, calc_hit},
         {object, Self},
         {props, Props},
         {character, Character},
         {hit, Miss},
         {target, Target},
         {result, succeed}]),
    % TODO: say "you missed!"
    Props;

succeed({Props, {Character, calc, Damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    log([{type, calc_damage},
         {object, Self},
         {props, Props},
         {character, Character},
         {damage, Damage},
         {target, Target},
         {result, succeed}]),
    erlmud_object:attempt(self(), {Character, does, Damage, to, Target, with, Self}),
    Props;

succeed({Props, {Character, calc, NoDamage, to, Target, with, Self}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    log([{type, calc_damage},
         {object, Self},
         {props, Props},
         {character, Character},
         {damage, NoDamage},
         {target, Target},
         {result, succeed}]),
    Props;

succeed({Props, {Character, stop_attack}}) ->
    log([{type, stop_attack},
         {props, Props},
         {character, Character},
         {result, succeed}]),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false});

succeed({Props, {Character, die}}) ->
    log([{type, die},
         {props, Props},
         {character, Character},
         {result, succeed}]),
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false});

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

attack(Props) ->
    Character = proplists:get_value(character, Props),
    Target = proplists:get_value(target, Props),
    Hit = proplists:get_value(attack_hit_modifier, Props, 1),
    erlmud_object:attempt(self(), {Character, calc, Hit, on, Target, with, self()}).

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
