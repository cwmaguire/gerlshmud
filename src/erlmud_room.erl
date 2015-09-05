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
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(_Props, _Owner, Pid) ->
    "room_" ++ Pid.

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(_Owner, Props, Msg) ->
    attempt(Props, Msg).

attempt(Props, {move, Obj, Self, Target}) when Self == self() ->
    log([Obj, <<" wants to go to ">>, Target, <<" from here">>]),
    {succeed, true, Props};
attempt(Props, {move, Obj, Source, Self}) when Self == self() ->
    log([Obj, <<" wants to come here from ">>, Source]),
    {succeed, true, Props};
attempt(Props, {get, Obj, Pid}) when is_pid(Pid) ->
    log([Obj, <<" wants to get ">>, Pid]),
    case has_pid(Props, Pid) of
        true ->
            log([Obj, <<" resending {get, ">>, Obj, <<", ">>, Pid, <<"} as {get, ">>, Obj, <<", ">>, Pid, <<", ">>, self(), <<"}">>]),
            {{resend, Obj, {get, Obj, Pid, self()}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(Props, Msg) ->
    log([<<"ignoring attempt ">>, Msg]),
    {succeed, false, Props}.

succeed(Props, {move, Obj, Self, Target}) when Self == self() ->
    log([Obj, <<", ">>, Target]),
    Props;
succeed(Props, {move, Obj, Source, Self}) when Self == self() ->
    log([Obj, <<" came from ">>, Source]),
    Props;
succeed(Props, {move, Obj, Source, Target}) ->
    log([<<"Process ">>, Obj, <<" went from ">>, Source, <<" to ">>, Target]),
    Props;
succeed(Props, {get, Obj, Item, Self}) when Self == self() ->
    log([<<"Process ">>, Obj, <<" got ">>, Item, <<" from me">>]),
    Props;
succeed(Props, Msg) ->
    log([<<"saw ">>, Msg, <<" succeed with props ">>, Props]),
    Props.

fail(Props, Reason, {move, Obj, Self, Target}) when Self == self() ->
    log([Obj, <<" couldn't go from here to ">>, Target, <<" ">>, Reason]),
    Props;
fail(Props, Reason, {move, Obj, Source, Self}) when Self == self() ->
    log([<<"Room ">>, Self, <<": ">>, Obj, <<" couldn't come here from ">>, Source, <<"Reason: ">>, Reason, <<", Props: ">>, Props]),
    Props.

log(Terms) ->
    erlmud_event_log:log([?MODULE | Terms]).
