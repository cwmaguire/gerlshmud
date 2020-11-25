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
-module(gerlshmud_handler_defend).
-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").


%% Defend
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, HitRoll, on, Character, with, AttackType}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, HitRoll},
           {?TARGET, Character},
           {attack_type, AttackType}],
    case should_defend(Props) of
        true ->
            %case gerlshmud_modifiers:modifier(Props, defence, hit, AttackType) of
            case proplists:get_value(defence_hit_roll, Props, {0, 0}) of
                {0, 0} ->
                    {succeed, false, Props, Log};
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    {succeed,
                     {Attacker, calc, HitRoll - DefenceRoll, on, Character, with, AttackType},
                     true,
                     Props,
                     Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, EffectRoll, on, Character, with, Effect}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_damage},
           {effect_roll, EffectRoll},
           {?TARGET, Character},
           {effect, Effect}],
    case should_defend(Props) of
        true ->
            case proplists:get_value(defence_effect_roll, Props, {0, 0}) of
                {_Roll = 0, _Base = 0} ->
                    {succeed, false, Props, Log};
                {MaybeRoll, Base} ->
                    DefenceRoll = roll(MaybeRoll, Base),
                    {succeed,
                     {Attacker, calc, EffectRoll - DefenceRoll, damage, Character},
                     true,
                     Props,
                     Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

should_defend(Props) ->
    ShouldDefend = proplists:get_value(should_defend_module, Props),
    ShouldDefend(Props).

roll(_Roll = 0, Base) ->
    Base;
roll(Roll, Base) ->
    rand:uniform(Roll) + Base.

%% log(Props) ->
%%     gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).
