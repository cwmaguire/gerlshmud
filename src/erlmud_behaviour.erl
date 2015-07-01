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
-module(erlmud_behaviour).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, Msg = {damage, _Att, _Src, Owner, _Dmg}) ->
    log("subscribed attempt: ~p, props: ~p~n", [Msg, Props]),
    {succeed, true, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {damage, _Att, Src, Owner}) ->
    erlmud_object:attempt_after(1000,
                                Owner
                                {attack, Self, Source, Target, 1}),
    Props;
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    log("result: ~p message: ~p~n", [Result, Msg]),
    Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
