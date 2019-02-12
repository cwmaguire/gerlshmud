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
-module(erlmud_handler_effect_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({_Parents,
         Props,
         {Self, affect, Target}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, affect},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({_Parents,
         Props,
         {Character, calc, Types, affect, Success, on, Target, with, Self}})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, calc_affect},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({_Parents,
         Props,
         {Character, calc, Types, damage, Damage, to, Target, with, Self}})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, calc_damage},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Self, affect, Target}}) ->
    Log = [{?EVENT, affect},
           {?SOURCE, Self},
           {?TARGET, Target},
           {handler, ?MODULE}],
    Character = proplists:get_value(character, Props),
    AttackTypes = proplists:get_value(attack_types, Props),
    PossibleSuccess = proplists:get_value(attack_hit, Props),
    Modifier = erlmud_modifiers:modifier(Props, attack, hit, AttackTypes),
    Success = rand:uniform(PossibleSuccess) + Modifier,
    Event = {Character,
             calc, AttackTypes,
             affect, Success,
             on, Target,
             with, Self},
    erlmud_object:attempt(self(), Event),
    {Props, Log};

succeed({Props, {Character, calc, Types, affect, Success, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Success > 0 ->
    PossibleDamage = proplists:get_value(attack_damage, Props, 0),
    Modifier = erlmud_modifiers:modifier(Props, attack, damage, Types),
    Damage = rand:uniform(PossibleDamage) + Modifier,
    Log = [{?SOURCE, Character},
           {?EVENT, calc_hit},
           {damage, Damage},
           {?TARGET, Target},
           {handler, ?MODULE},
           {vector, Self}],
    Event = {Character, calc, Types, damage, Damage, to, Target, with, Self},
    erlmud_object:attempt(self(), Event),
    {Props, Log};

succeed({Props, {Character, calc, Types, effect, Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Target},
           {?EVENT, calc_success},
           {handler, ?MODULE},
           {success, Miss},
           {effect, Self},
           {types, Types}],
    % TODO fill in what target they missed and with what effect
    erlmud_object:attempt(Character, {send, Character, <<"You missed!">>}),
    {Props, Log};

succeed({Props, {Character, calc, Types, damage, Damage, to, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{?SOURCE, Self},
           {?TARGET, Target},
           {?EVENT, calc_damage},
           {handler, ?MODULE},
           {damage, Damage},
           {types, Types}],
    DamageEvent = {Character, does, Types, damage, Damage, to, Target, with, Self},
    lager:info("~p sending event ~p~n", [?MODULE, DamageEvent]),
    erlmud_object:attempt(self(), DamageEvent),

    Desc = proplists:get_value(desc, Props, <<"no description">>),
    DamageBin = integer_to_binary(Damage),
    % TODO have the connection object figure out the missing information
    % by sending messages out to owners, e.g. {Target, name}
    Msg = <<"You hit <target> with ", Desc/binary,
            " for ", DamageBin/binary, " damage">>,
    erlmud_object:attempt(Character, {send, Character, Msg}),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

%log(Props) ->
    %erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).

