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
-module(erlmud_hitpoints).

-behaviour(erlmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(_Props, Owner, Pid) ->
    "hp_of_" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, {damage, _Att, _Src, Owner, _Dmg}) ->
    {succeed, true, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, Msg = {damage, Attack, Source, Owner, Damage}) ->
    log([<<"saw ">>, Msg, <<"succeed">>]),
    take_damage(Attack, Source, Owner, Damage, Props);
succeed(Props, _Msg) ->
    Props.

fail(Props, Message, _Reason) ->
    log([<<"saw ">>, Message, <<" fail with props ">>, Props]),
    Props.

take_damage(Attack, Source, Owner, Damage, Props) ->
    log([<<"taking ">>, Damage, <<" points of damage.">>]),
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            log([<<"dying; hp = ">>, Hp]),
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {killed, Attack, Source, Owner});
        _ ->
            log([<<"Not dying; hitpoints = ">>, Hp]),
            ok
    end,
    lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}).

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
