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

attempt(Props, {Action, Obj, [_ | _] = PartialItemName}) when Action == get; Action == drop ->
    case re:run(proplists:get_value(name, Props, ""), PartialItemName, [{capture, none}]) of
        match ->
            io:format("~p resending {~p, ~p, ~p} as {~p, ~p, ~p}~n",
                      [?MODULE, Action, Obj, PartialItemName, Action, Obj, self()]),
            {{resend, Obj, {Action, Obj, self()}}, true, Props};
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
attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {get, Receiver, Self, Owner}) when Self == self() ->
    move(Props, Owner, Receiver);
succeed(Props, {drop, Owner, Self, Receiver}) when Self == self() ->
    move(Props, Owner, Receiver);
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    Props.

move(Props, Owner, Receiver) ->
    io:format("Item ~p: moving from ~p to ~p~n", [self(), Owner, Receiver]),
    gen_server:cast(Owner, {remove, item, self()}),
    gen_server:cast(Receiver, {add, item, self()}),
    set(owner, Receiver, Props).

log(Msg, Props) ->
    io:format("Item ~p received: ~p with props: ~p~n",
              [self(), Msg, Props]).
