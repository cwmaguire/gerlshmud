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

attempt({_Parents,
         Props,
         {Self, affect, Target}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, affect},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({_Parents,
         Props,
         {_Character, _Roll, for, EffectType, on, Target, with, Self}})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    {succeed, true, Props, Log};

attempt({_Parents,
         Props,
         {_Character, _Roll, for, EffectType, on, Target, with, Self}})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, roll_for_effect},
           {?TARGET, Target},
           {effect_type, EffectType}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Self, affect, Target}}) ->
    Log = [{?EVENT, affect},
           {?SOURCE, Self},
           {?TARGET, Target},
           {handler, ?MODULE}],
    Character = proplists:get_value(character, Props),
    EffectType = proplists:get_value(effect_type, Props),
    EffectRoll = proplists:get_value(effect_roll, Props),
    EffectRollBase = proplists:get_value(effect_roll_base, Props),
    Roll = EffectRollBase + rand:uniform(EffectRoll),
    NewMessage = {Character, Roll, for, EffectType, on, Target, with, Self},
    gerlshmud_object:attempt(self(), NewMessage),
    {Props, Log};

succeed({Props, {Character, calc, EffectType, EffectAmount, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       EffectAmount > 0 ->
    Log = [{?SOURCE, Character},
           {?EVENT, roll_for_effect},
           {amount, EffectAmount},
           {?TARGET, Target},
           {handler, ?MODULE},
           {effect, Self}],
    Event = {Character, cause, EffectType, EffectAmount, on, Target, with, Self},
    gerlshmud_object:attempt(self(), Event),
    {Props, Log};

% Don't say anything if nothing happens
% Only say something if _something_ happens
% succeed({Props, {Character, FailedRoll, for, EffectType, on, Target, with, Self}})
%   when is_pid(Target),
%        Self == self() ->
%     Log = [{?SOURCE, Self},
%            {?TARGET, Target},
%            {?EVENT, affect},
%            {handler, ?MODULE},
%            {roll, FailedRoll},
%            {effect, Self},
%            {effect_type, EffectType}],
%     gerlshmud_object:attempt(Character, {send, Character, <<"You missed!">>}),
%     {Props, Log};

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

