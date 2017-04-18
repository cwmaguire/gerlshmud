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

%% Someone has damaged us
attempt({#parents{},
         Props,
         {_Attacker, does, _Damange, to, Self, with, _AttackVector}})
  when Self == self() ->
    log([<<"caught damager attempt">>]),
    {succeed, true, Props};

%% Someone is intending to attack us
attempt({#parents{}, Props, {_Attacker, attack, Self}}) when Self == self() ->
    log([<<"caught attack attempt">>]),
    {succeed, true, Props};

attempt({_Owner, _Props, _Attempt}) ->
    %log([self(), <<" caught attempt but not subscribing">>, Attempt]),
    undefined.

succeed({Props, {Attacker, does, _Damage, to, _Character, with, AttackVector}}) ->
    Target = proplists:get_value(target, Props),
    _ = case is_pid(Target) of
        false ->
            log([<<"no attacks yet, attack back props: ">>, Props]),
            erlmud_object:attempt(self(), {self(), counter_attack, Attacker});
        true ->
            log([<<"already attacking ">>, Target, <<" with ">>, AttackVector, <<". Stick with it. Props: ">>, Props]),
            ok
    end,
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, Result, Msg}) ->
    log([<<"result: ">>, Result, <<" message: ">>, Msg]),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
