%% Copyright (c) 2019, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_handler_effect_attack).
-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATTEMPT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attempt({_Parents,
         Props,
         {Self, affect, Target}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, affect},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({#parents{},
         Props,
         {Resource, allocate, Required, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Required},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    {succeed, true, Props, Log};

attempt({_Parents,
         Props,
         {_Character, roll, _Roll, for, HitOrEffect, with, EffectType, on, Target, with, Self}})
  when Self == self(),
       HitOrDmg == hit; HitOrEffect == Effect ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUCCEED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeed({Props, {Self, affect, Target}}) ->
    Log = [{?EVENT, affect},
           {?SOURCE, Self},
           {?TARGET, Target},
           {handler, ?MODULE}],
    Character = proplists:get_value(character, Props),
    reserve(Character, Props),
    {Props, Log};

succeed({Props, {Resource, allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    Allocated = update_allocated(Amt, Type, Props),
    Required = proplists:get_value(resources, Props, []),
    HasResources = has_resources(Allocated, Required),
    RemainingAllocated =
        case HasResources of
            true ->
                affect(Props),
                deallocate(Allocated, Required);
            _ ->
                Allocated
        end,
    Props2 = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated}),
    {Props2, Log};

succeed({Props, {Character, roll, SuccessRoll, for, hit, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self()
       SuccessRoll > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    gerlshmud_object:attempt(self(), Event);
    {Props, Log};

succeed({Props, {Character, roll, FailRoll, for, hit, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (itob(Roll))/binary, "]">>.
    CharacterMsg = <<"You miss <target> with ",
            (gerlshmud_util:atob(EffectType))/binary,
            AmountBin/binary>>,
    gerlshmud_object:attempt(Target, {send, Character, CharacterMsg, Substitutions}),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character> misses you with ", (gerlshmud_util:atob(EffectType))/binary>>,
    Substitutions = [{<<"<target>">>, Target},
                     {<<"<character>">>, Character},
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, Substitutions}),
    {Props, Log};

succeed({Props, {Character, roll, Roll, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       EffectAmount > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],
    %Event = {Character, cause, EffectType, EffectAmount, on, Target, with, Self},
    Event = {Character, affect, Target, with, EffectType, doing, EffectAmount, with, Self},
    gerlshmud_object:attempt(self(), Event),

    %% TODO go again?
    {Props, Log};

succeed({Props, {Character, roll, FailRoll, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (itob(Roll))/binary, "]">>.
    CharacterMsg = <<(atob(EffectType))/binary, " has no effect on <target>",
            (gerlshmud_util:atob(EffectType))/binary,
            AmountBin/binary>>,
    gerlshmud_object:attempt(Target, {send, Character, CharacterMsg, Substitutions}),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character>'s ", (gerlshmud_util:atob(EffectType))/binary, " has no effect">>,
    Substitutions = [{<<"<target>">>, Target},
                     {<<"<character>">>, Character},
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, Substitutions}),
    {Props, Log};

reserve(Character, Props) when is_list(Props) ->
    [reserve(Character, Resource, Amount) || {Resource, Amount} <- proplists:get_value(resources, Props, [])].

reserve(Character, Resource, Amount) ->
    gerlshmud_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, self()}).

succeed({Props, {Resource, allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
    Log = [{?EVENT, allocate},
           {amount, Amt},
           {resource_type, Type},
           {?SOURCE, Resource},
           {?TARGET, Self}],
    Allocated = update_allocated(Amt, Type, Props),
    Required = proplists:get_value(resources, Props, []),
    HasResources = has_resources(Allocated, Required),
    RemainingAllocated =
        case HasResources of
            true ->
                affect(Props),
                deallocate(Allocated, Required);
            _ ->
                Allocated
        end,
    Props2 = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated}),
    {Props2, Log};

update_allocated(New, Type, Props) ->
    Allocated = proplists:get_value(allocated_resources, Props, #{}),
    Curr = maps:get(Type, Allocated, 0),
    Allocated#{Type => Curr + New}.

has_resources(Allocated, Required) ->
    {_, AllocApplied} = lists:foldl(fun apply_resource/2, {Allocated, []}, Required),
    case lists:filter(fun is_resource_lacking/1, AllocApplied) of
        [] ->
            true;
        _ ->
            false
    end.

update_allocated(New, Type, Props) ->
    Allocated = proplists:get_value(allocated_resources, Props, #{}),
    Curr = maps:get(Type, Allocated, 0),
    Allocated#{Type => Curr + New}.

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

affect(Props) ->
    Character = proplists:get_value(character, Props),
    EffectType = proplists:get_value(effect_type, Props),
    Target = proplists:get_value(target, Target),
    Roll = calc_hit_roll(Props),
    NewMessage = {Character, Roll, for, EffectType, on, Target, with, self()},
    gerlshmud_object:attempt(self(), NewMessage),


    Target = proplists:get_value(target, Props),
    AttackType = proplists:get_value(attack_type, Props, []),
    Hit = calc_hit(Props),

    Event = {Character, roll, 0, for, affect, with, EffectType, on, Target, with, Self}
    % I think we're auto-subscribed
    gerlshmud_object:attempt(self(), Event).

calc_hit_roll(Props) ->
    Roll = proplists:get_value(effect_hit_roll, Props, {0, 0}),
    gerlshmud_roll:roll(Roll).

deallocate(Allocated, Required) ->
    lists:foldl(fun subtract_required/2, Allocated, Required).

subtract_required({Type, Required}, Allocated) ->
    #{Type := Amt} = Allocated,
    Allocated#{Type := min(0, Amt - Required)}.

succeed({Props, {Character, affect, Target, with, Roll, EffectType, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Target},
           {?EVENT, affect},
           {handler, ?MODULE},
           {roll, Roll},
           {effect_type, EffectType}],
    EffectEvent = {Character, affects, Target, with, Roll, EffectType, with, Self},
    lager:info("~p sending event ~p~n", [?MODULE, EffectEvent]),
    gerlshmud_object:attempt(self(), EffectEvent),

    RollBin = integer_to_binary(Roll),
    % TODO have the connection object figure out the missing information
    % by sending messages out to owners, e.g. {Target, name}
    Msg = [Character, <<" does ", RollBin/binary, " of ">>,
           EffectType, <<" to ">>, Target],
    gerlshmud_object:attempt(Character, {send, Character, Target, Msg}),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

%log(Props) ->
    %gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).

