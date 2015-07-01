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
-module(erlmud_ai).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(_Owner, Props, Msg) ->
    attempt(Props, Msg).

attempt(Props, {attack, Attack, Attacker, Name}) when is_list(Name) ->
    log("Checking if name ~p matches", [Name]),
    case re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}]) of
        match ->
            log("resending {attack, ~p, ~p} as {attack, ~p, ~p}~n",
                [Attacker, Name, Attacker, self()]),
            {{resend, Attack, {attack, Attack, Attacker, self()}}, true, Props};
        _ ->
            log("Name ~p did not match.~n\tProps: ~p~n", [Name, Props]),
            {succeed, false, Props}
    end;
attempt(Props, {calc_hit, Attack, Attacker, Self, _}) when Self == self() ->
    case proplists:get_value(hp, Props) of
        X when X < 0 ->
            {{resend, Attacker, {killed, Attack, Attacker, self()}}, true, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(Props, Msg) ->
    log("ignored attempt: ~p~nProps: ~p~n", [Msg, Props]),
    {succeed, false, Props}.

succeed(_Props, {cleanup, Self}) when Self == self() ->
    %% TODO: drop all objects
    %% TODO: kill all connected processes
    exit(cleanup);
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
