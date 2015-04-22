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

attempt(Props, {get, Obj, Item}) ->
    case is_item(Props, Item) of
        true ->
            {{resend, Obj, {get, Obj, self()}}, true, Props};
        false ->
            {succeed, false, Props}
    end;
attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {move, Self, Source, Target}) when Self == self() ->
    io:format("Item ~p: moving from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, item, self()}),
    gen_server:cast(Target, {add, item, self()}),
    set(holder, Target, Props);
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    Props.

log(Msg, Props) ->
    io:format("Item ~p received: ~p with props: ~p~n",
              [self(), Msg, Props]).

is_item(Props, Item) ->
    Name = proplists:get_value(name, Props),
    has_name(Name, Item).

has_name(undefined, _) ->
    false;
has_name(TooSmall, Name) when length(Name) > length(TooSmall) ->
    false;
has_name(Name, ItemName) ->
    case string:substr(Name, 1, length(ItemName)) of
        ItemName ->
            true;
        _ ->
             has_name(tl(Name), ItemName)
    end.
