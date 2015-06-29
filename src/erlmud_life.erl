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
-module(erlmud_life).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

is_dead_action(revive) ->
    true;
is_dead_action(_) ->
    false.

attempt(Props, Msg) ->
    Owner = proplists:get_value(owner, Props),
    IsAlive = proplists:get_value(is_alive, Props, false),
    IsDeadAction = is_dead_action(element(1, Props)),
    MsgOwner = element(2, Msg),
    case Owner == MsgOwner andalso
         (IsAlive orelse
          IsDeadAction) of
        true ->
            {succeed, _Subscribe = false, Props};
        false ->
            {fail, _Subscribe = false, Props}
    end.

succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    throw(should_never_happen).
    %Props.

fail(Props, Message, _Reason) ->
    log("saw ~p fail with props ~p~n", [Message, Props]),
    throw(should_never_happen).
    %Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
