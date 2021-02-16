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
         {Character, affect, Target, because, Attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, affect},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

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

succeed({Props, {Character, attack, Target, beacuse, Attack}}) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {vector, Attack}],
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

    TargetSubstitutions = [{<<"<character>">>, Character}],
    TargetMsg = <<"<character> misses you with ", (gerlshmud_util:atob(EffectType))/binary>>,
    TargetSubstitutions = [{<<"<target>">>, Target},
                           {<<"<character>">>, Character}],
    gerlshmud_object:attempt(Target, {send, Target, TargetMsg, TargetSubstitutions}),
    {Props, Log};

succeed({Props, {Character, roll, EffectAmount, for, effect, with, _EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       EffectAmount > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    maybe_repeat(Props),
    {Props, Log};

succeed({Props, {Character, roll, EffectAmount, for, effect, with, EffectType, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],

    CharacterSubstitutions = [{<<"<target>">>, Target}],
    AmountBin = <<" [", (gerlshmud_util:itob(EffectAmount))/binary, "]">>,
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
maybe_repeat(_Props) ->
    ok.


%log(Props) ->
    %gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).

