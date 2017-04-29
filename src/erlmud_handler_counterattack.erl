%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_counterattack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({#parents{}, Props, {_Attacker, attack, Self}}) when Self == self() ->
    log([<<"caught attack attempt of ">>, _Attacker, attack, Self]),
    {succeed, true, Props};

attempt({#parents{}, Props, {Self, attack, _Target}}) when Self == self() ->
    {succeed, true, Props};

attempt({#parents{}, _, _}) ->
    undefined.

succeed({Props, {_Character, stop_attack}}) ->
    _Props = lists:keystore(is_attacking, 1, Props, {is_attacking, false});

succeed({Props, {Attacker, attack, Target}}) ->
    case proplists:get_value(owner, Props) of
        Attacker ->
            _Props = lists:key_store(is_attacking, 1, Props, {is_attacking, true});
        Target ->
            case proplists:get_value(is_attacking, Props) of
                true ->
                    ok;
                _ ->
                    erlmud_object:attempt(self(), {self(), counter_attack, Attacker})
            end,
            Props;
        _ ->
            Props
    end;

succeed({Props, {_Self, counter_attack, Target}}) ->
    erlmud_object:attempt(self(), {self(), attack, Target}),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, Result, Msg}) ->
    log([<<"result: ">>, Result, <<" message: ">>, Msg]),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
