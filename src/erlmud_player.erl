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
-module(erlmud_player).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).

get_(Type, Props) ->
    lists:keyfind(Type, 1, Props).

attempt(Props, {move, Self, Direction}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
attempt(Props, {enter_world, Self}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room when is_pid(Room) ->
            {succeed, true, Props}
    end;
attempt(Props, {drop, Self, Pid}) when Self == self(), is_pid(Pid) ->
    log("~p attempting to drop ~p~n", [Self, Pid]),
    case has_pid(Props, Pid) of
        true ->
            {room, Room} = get_(room, Props),
            log("resending {drop, ~p, ~p} as {drop, ~p, ~p, ~p}~n",
                [Self, Pid, Self, Pid, Room]),
            {{resend, Self, {drop, Self, Pid, Room}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(Props, {calc_next_attack_wait, Attack, Self, Target, Sent, Wait})
    when Self == self() ->
    PlayerWait = proplists:get_value(attack_wait, Props, 0),
    log("Player attack wait is ~p~n", [PlayerWait]),
    {succeed,
     {calc_next_attack_wait, Attack, Self, Target, Sent, Wait + PlayerWait},
     false,
     Props};
attempt(Props, {move, Self, _, _}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, {attack, Self, _}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, Msg) ->
    log("attempt: ~p~nProps: ~p~n", [Msg, Props]),
    {succeed, false, Props}.

succeed(Props, {move, Self, Source, Target}) when Self == self(), is_pid(Target) ->
    log("moved from ~p to ~p~n", [Source, Target]),
    erlmud_object:remove(Source, player, self()),
    erlmud_object:add(Target, player, self()),
    set(room, Target, Props);
succeed(Props, {move, Self, Source, Direction}) when Self == self(), is_atom(Direction) ->
    log("succeeded in moving ~p from ~p~n", [Direction, Source]),
    Props;
succeed(Props, {enter_world, Self})
    when Self == self() ->
    Room = proplists:get_value(room, Props),
    log("entering ~p~n", [Room]),
    erlmud_object:add(Room, player, self());
succeed(Props, {get, Self, Source, Item}) when Self == self() ->
    log("getting ~p from ~p~n\tProps: ~p~n", [Item, Source, Props]),
    Props;
succeed(Props, {attack, Self, Target}) when Self == self() ->
    log("{attack, self(), ~p} succeeded~n"
        "Starting attack process~n"
        "\tProps: ~p~n",
        [Target, Props]),
    attack(Target, stop_attack(Props));
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

stop_attack(Props) ->
    case lists:keytake(attack, 1, Props) of
        {Pid, _, Props2} ->
            exit(Pid),
            Props2;
        _ ->
            Props
    end.

attack(Target, Props) ->
    {ok, Attack} = supervisor:start_child(erlmud_object_sup,
                                          [undefined, erlmud_attack, [{player, self()}]]),
    log("Attack ~p started, sending attempt~n", [Attack]),
    erlmud_object:attempt(Attack, {attack, Attack, self(), Target}),
    [{attack, Attack} | Props].

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
