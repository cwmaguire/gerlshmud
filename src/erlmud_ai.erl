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
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Props, {hit, Attack, Attacker, Name}) when is_list(Name) ->
    case re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}]) of
        match ->
            io:format("~p resending {hit, ~p, ~p} as {hit, ~p, ~p}~n",
                      [?MODULE, Attacker, Name, Attacker, self()]),
            {{resend, Attack, {hit, Attack, Attacker, self()}}, true, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {damage, _Attack, _Source, Self, Damage}) when Self == self() ->
    take_damage(Damage, Props);
succeed(Props, {die, Self}) when Self == self() ->
    CorpseCleanupMilis = application:get_env(erlmud, corpse_cleanup_milis, 10 * 60 * 1000),
    erlmud_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Self}),
    Props;
succeed(_Props, {cleanup, Self}) when Self == self() ->
    exit(cleanup);
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

log(Msg, Props) ->
    io:format("~p received: ~p props: ~p~n", [?MODULE, Msg, Props]).

take_damage(Damage, Props) ->
    io:format("~p ~p took ~p damage~nProps: ~p~n", [?MODULE, self(), Damage, Props]),
    Hp = proplists:get_value(hp, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            erlmud_object:attempt(self(), {die, self()});
        _ ->
            ok
    end,
    lists:keystore(hp, 1, Props, {hp, Hp}).
