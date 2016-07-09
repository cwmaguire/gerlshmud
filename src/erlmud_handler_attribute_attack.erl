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
-module(erlmud_handler_attribute_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({Owner, Props, {CalcType, Attack, Source, Target, Value}})
  when CalcType == calc_hit; CalcType == calc_damage ->
    Character = proplists:get_value(character, Props),
    case {Owner, Character} of
        {Owner, Character} when Owner == Source; Character == Source ->
            log(debug, [self(), <<": either Source (">>, Source, <<") ">>,
                        <<"is our Owner (">>, Owner, <<") ">>,
                        <<"or our character (">>, Character, <<")">>]),
            modify_msg(CalcType, Attack, Source, Target, source, Value, Props);
        {Owner, Character} when Owner == Target; Character == Target ->
            log(debug, [self(), <<": either Target (">>, Target, <<") ">>,
                        <<"is our Owner (">>, Owner, <<") ">>,
                        <<"or our character (">>, Character, <<")">>]),
            modify_msg(CalcType, Attack, Source, Target, target, Value, Props);
        _ ->
           log(debug, [<<"attribute attack ">>, CalcType,
                       <<" attempt failed since neither owner (">>, Owner,
                       <<") nor character (">>,
                       proplists:get_value(character, Props),
                       <<") are equal to ">>, Source, <<" or ">>, Target]),
           undefined
    end;
attempt({_, _, _Msg}) ->
    %log(debug, [<<"erlmud_handler_attribute_attack did not handle ">>, Msg]),
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

modify_msg(CalcType, Attack, Source, Target, SourceOrTarget, Value, Props) ->
    Modifier = modifier(CalcType, SourceOrTarget, Props),
    log(debug, [self(), <<": Modifier is: ">>, Modifier]),
    UpdatedValue = Value + Modifier,
    UpdatedMsg = {CalcType, Attack, Source, Target, UpdatedValue},
    {succeed, UpdatedMsg, false, Props}.

modifier(CalcType, SourceOrTarget, Props) ->
    {AttackModifierProp, DefenceModifierProp} =
        case CalcType of
            calc_hit ->
                {attack_hit_modifier, defence_hit_modifier};
            calc_damage ->
                {attack_damage_modifier, defence_damage_modifier}
        end,
    Modifier = case SourceOrTarget of
                   source ->
                       proplists:get_value(AttackModifierProp, Props, 0);
                   target ->
                       proplists:get_value(DefenceModifierProp, Props, 0)
               end,
    log(debug, [self(), <<": Modifier is: ">>, Modifier]),
    Modifier.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
