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
-module(erlmud_handler_char_inject_self).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%attempt({_Owner, Props, {Action, Obj, ItemName, BodyPart}})
attempt({#parents{}, Props, {Attacker, attack, TargetName}})
  when is_binary(TargetName) ->
    case is_name(Props, TargetName) of
        true ->
            AttackTypes = proplists:get_value(attack_types, Props),
            NewMessage = {Attacker, attack, self(), with, AttackTypes},
            Result = {resend, Attacker, NewMessage},
            {Result, true, Props};
        _ ->
            _Name = proplists:get_value(name, Props),
            %log(debug, [<<"Name ">>, Name, <<" did not match target name: ">>, TargetName]),
            {succeed, _Subscribe = false, Props}
    end;
attempt({Owner, Props, {look, Self}}) when Self == self() ->
    NewMessage = {look, Self, Owner},
    {{resend, Self, NewMessage}, _ShouldSubscribe = false, Props};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}, caseless]).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
