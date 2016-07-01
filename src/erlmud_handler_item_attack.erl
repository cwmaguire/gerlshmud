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

attempt({_Owner, Props, {calc_damage, Attack, Source, Target, Damage}}) ->
    case proplists:get_value(character, Props) of
        Source ->
            UpdatedDmg = Damage + proplists:get_value(attack_damage_modifier, Props, 0),
            UpdatedMsg = {calc_damage, Attack, Source, Target, UpdatedDmg},
            {succeed, UpdatedMsg, false, Props};
        Target ->
            UpdatedDmg = Damage + proplists:get_value(defense_damage_modifier, Props, 0),
            UpdatedMsg = {calc_damage, Attack, Source, Target, UpdatedDmg},
            {succeed, UpdatedMsg, false, Props};
        _ ->
           log(debug, [<<"item attack attempt failed since ">>,
                       proplists:get_value(character, Props),
                       <<" is not equal to ">>, Source,
                       <<" or ">>, Target]),

           undefined
    end;
attempt({_, _, Msg}) ->
   log(debug, [<<"erlmud_handler_item_attack did not handle ">>,
               Msg]),
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
