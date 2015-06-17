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
-module(erlmud_room).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Props, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p wants to go to ~p~n",
              [self(), Obj, Target]),
    {succeed, true, Props};
attempt(Props, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p wants to come from ~p~n", [self(), Obj, Source]),
    {succeed, true, Props};
attempt(Props, {get, Obj, Pid}) when is_pid(Pid) ->
    io:format("Room ~p: ~p wants to get ~p~n", [self(), Obj, Pid]),
    case has_pid(Props, Pid) of
        true ->
            io:format("~p resending {get, ~p, ~p} as {get, ~p, ~p, ~p}~n",
                      [?MODULE, Obj, Pid, Obj, Pid, self()]),
            {{resend, Obj, {get, Obj, Pid, self()}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(Props, Msg) ->
    io:format("Room ~p: ignoring attempt ~p~n", [self(), Msg]),
    {succeed, false, Props}.

succeed(Props, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p left for ~p~n", [self(), Obj, Target]),
    Props;
succeed(Props, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p came from ~p~n", [self(), Obj, Source]),
    Props;
succeed(Props, {move, Obj, Source, Target}) ->
    io:format("Room ~p: Process ~p went from ~p to ~p~n", [self(), Obj, Source, Target]),
    Props;
succeed(Props, {get, Obj, Item, Self}) when Self == self() ->
    io:format("Room ~p: Process ~p got ~p from me~n", [self(), Obj, Item]),
    Props;
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, Reason, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p couldn't go from here to ~p~n\t~p~n", [self(), Obj, Target, Reason]),
    Props;
fail(Props, Reson, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p couldn't come here from ~p~n\t~p~n", [self(), Obj, Source, Reson]),
    Props.
