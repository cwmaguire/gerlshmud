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
-module(erlmud_item).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).

is_name(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}]).

attempt(Props, {Action, Obj, ItemName, BodyPart})
  when is_list(ItemName) andalso
       (Action == add orelse
        Action == remove) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Action, Obj, self(), BodyPart},
            Result = {resend, Obj, NewMessage},
            {Result, true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(Props, {Action, Owner, ItemName}) when is_list(ItemName) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Action, Owner, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(Props, {Action, Obj, [_ | _] = ItemName})
  when Action == get;
       Action == drop ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Action, Obj, self()},
            Result = {resend, Obj, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(Props, {calc_damage, Attack, Source, Target, Damage}) ->
    case proplists:get_value(owner, Props) of
        Source ->
            UpdatedDmg = Damage + proplists:get_value(dmg, Props, 0),
            UpdatedMsg = {calc_damage, Attack, Source, Target, UpdatedDmg},
            {succeed, UpdatedMsg, false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(Props, Msg = {Action, _, Self, _}) when Self == self() ->
    log("subscribed attempt: ~p, props: ~p~n", [Msg, Props]),
    {succeed, lists:member(Action, [get, drop]), Props};
attempt(Props, _Msg) ->
    %log("ignored attempt: ~p, props: ~p~n", [Msg, Props]),
    {succeed, false, Props}.

succeed(Props, {get, Receiver, Self, Owner}) when Self == self() ->
    move(Props, Owner, Receiver);
succeed(Props, {drop, Owner, Self, Receiver}) when Self == self() ->
    move(Props, Owner, Receiver);
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    log("result: ~p message: ~p~n", [Result, Msg]),
    Props.

move(Props, Owner, Receiver) ->
    log("moving from ~p to ~p~n", [Owner, Receiver]),
    gen_server:cast(Owner, {remove, item, self()}),
    gen_server:cast(Receiver, {add, item, self()}),
    set(owner, Receiver, Props).

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
