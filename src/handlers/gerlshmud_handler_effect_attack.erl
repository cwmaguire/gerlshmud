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

attempt({#parents{owner = Attack},
         Props,
         {Self, affect, Target}})
  when Self == self() ->
    Log = [{?SOURCE, Attack},
           {?EVENT, affect},
           {?TARGET, Target}],
    ShouldSubscribe = proplists:get_value(target, Props, undefined) == Target,
    {succeed, ShouldSubscribe, Props, Log};

attempt({_Parents,
         Props,
         {_Character, roll, _Roll, for, HitOrEffect, with, EffectType, on, Target, with, Self}})
  when Self == self(),
       HitOrEffect == hit; HitOrEffect == effect ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    case proplists:get_value(type, Props) of
        EffectType ->
            {succeed, true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({_, _, _Msg}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUCCEED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeed({Props, {_Self, affect, Target}}) ->
    Attack = proplists:get_value(owner, Props),
    Log = [{?SOURCE, Attack},
           {?EVENT, affect},
           {?TARGET, Target},
           {vector, Attack}],
    Character = proplists:get_value(character, Props),
    EffectType = proplists:get_value(type, Props),
    Event = {Character, roll, calc_hit_roll(Props), for, hit, with, EffectType, on, Target, with, self()},
    gerlshmud_object:attempt(self(), Event),

    {Props, Log};

succeed({Props, {Character, roll, SuccessRoll, for, hit, with, EffectType, on, Target, with, Attack}})
  when is_pid(Target),
       SuccessRoll > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, SuccessRoll},
           {?TARGET, Target},
           {handler, ?MODULE},
           {vector, Attack},
           {effect_type, EffectType},
           {effect, self()}],

    Event = {Character, roll, calc_effect_roll(Props), for, effect, with, EffectType, on, Target, with, self()},
    gerlshmud_object:attempt(self(), Event),
    {Props, Log};

succeed({Props, {Character, roll, FailRoll, for, hit, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_hit},
           {amount, FailRoll},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (gerlshmud_util:itob(FailRoll))/binary, "]">>,
    CharacterMsg =
        <<"You miss <target> with ",
          (gerlshmud_util:atob(EffectType))/binary,
          AmountBin/binary>>,
    gerlshmud_object:attempt(Target, {send, Character, CharacterMsg, CharacterSubstitutions}),
    ct:pal("~p: CharacterMsg~n\t~p~n", [?MODULE, CharacterMsg]),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character> misses you with ", (gerlshmud_util:atob(EffectType))/binary>>,
    TargetSubstitutions = [{<<"<target>">>, Target},
                           {<<"<character>">>, Character}],
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}),
    ct:pal("~p: TargetMsg~n\t~p~n", [?MODULE, TargetMsg]),
    {Props, Log};

succeed({Props, {Character, roll, EffectAmount, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       EffectAmount > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    EffectEvent = {Character, cause, EffectAmount, 'of', EffectType, to, Target, with, Self},
    gerlshmud_object:attempt(Target, EffectEvent, false),

    maybe_repeat(Props, Log);

succeed({Props, {Character, roll, IneffectiveAmount, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, IneffectiveAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (gerlshmud_util:itob(IneffectiveAmount))/binary, "]">>,
    CharacterMsg =
        <<(gerlshmud_util:atob(EffectType))/binary,
          " has no effect on <target> (",
          AmountBin/binary,
          ")">>,
    gerlshmud_object:attempt(Target, {send, Character, CharacterMsg, CharacterSubstitutions}),

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character>'s ", (gerlshmud_util:atob(EffectType))/binary, " has no effect">>,
    TargetSubstitutions = [{<<"<target>">>, Target},
                           {<<"<character>">>, Character}],
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}),
    {Props, Log};

succeed({Props, {Attacker, do, EffectAmount, 'of', EffectType, to, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Target},
           {?EVENT, affect},
           {handler, ?MODULE},
           {effect_type, EffectType}],

    AttackerSubstitutions = [{<<"<target>">>, self()}],
    AmountBin = <<" [", (gerlshmud_util:itob(EffectAmount))/binary, "]">>,
    AttackerMsg =
        <<"You do ",
          AmountBin/binary,
          " damage to <target> with ",
          (gerlshmud_util:atob(EffectType))/binary>>,
    gerlshmud_object:attempt(Attacker, {send, Attacker, AttackerMsg, AttackerSubstitutions}, _Sub = false),

    TargetSubstitutions = [{<<"<attacker>">>, Attacker}],
    TargetMsg = <<"<attacker> does ",
                  AmountBin/binary,
                  " damage to you with ",
                  (gerlshmud_util:atob(EffectType))/binary>>,
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}, _Sub = false),


    {Props, Log};

succeed({Props, {stop, Self}}) when Self == self() ->
    {stop, finished, Props, _LogProps = []};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

%% TODO move to target processes: e.g. HP, stamina, etc.
% affect(Props, EffectAmount) ->
%     Character = proplists:get_value(character, Props),
%     EffectType = proplists:get_value(effect_type, Props),
%     Target = proplists:get_value(target, Props),
%     NewMessage = {Character, Roll, for, EffectType, on, Target, with, self()},
%     gerlshmud_object:attempt(self(), NewMessage),
%
%    Target = proplists:get_value(target, Props),
%    AttackType = proplists:get_value(attack_type, Props, []),
%    Hit = calc_hit(Props),
%
%    Event = {Character, roll, 0, for, affect, with, EffectType, on, Target, with, Self},
%    % I think we're auto-subscribed
%    gerlshmud_object:attempt(self(), Event).

calc_hit_roll(Props) ->
    Roll = proplists:get_value(hit_roll, Props, {0, 0}),
    gerlshmud_roll:roll(Roll).

calc_effect_roll(Props) ->
    EffectAmount = proplists:get_value(effect_roll, Props, 0),
    gerlshmud_roll:roll(EffectAmount).

% TODO implement repeat logic for effects that keep going
% maybe re-roll for hit?
% maybe check wait time?
maybe_repeat(Props, Log) ->
    stop(Props, Log).

stop(Props, Log) ->
    Owner = proplists:get_value(owner, Props),
    StopEvent = {delete, self()},
    gerlshmud_object:attempt(Owner, StopEvent),
    {stop, finished, Props, Log}.


%log(Props) ->
    %gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).

