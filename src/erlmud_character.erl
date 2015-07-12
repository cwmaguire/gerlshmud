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
-module(erlmud_character).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
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

attempt(_Owner, Props, Msg) ->
    attempt(Props, Msg).

attempt(Props, {move, Self, Direction}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Character doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
attempt(Props, {enter_world, Self}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Character doesn't have room"}, false, Props};
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
attempt(Props, {calc_next_attack_wait, Attack, Self, Target, Sent, Wait})
    when Self == self() ->
    CharacterWait = proplists:get_value(attack_wait, Props, 0),
    log("Character attack wait is ~p~n", [CharacterWait]),
    {succeed,
     {calc_next_attack_wait, Attack, Self, Target, Sent, Wait + CharacterWait},
     false,
     Props};
attempt(Props, {move, Self, _, _}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, {attack, Self, _}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, {stop_attack, Attack}) ->
    {succeed, _IsCurrAttack = lists:member({attack, Attack}, Props), Props};
attempt(Props, {die, Self}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, Msg) ->
    log("attempt: ~p~nProps: ~p~n", [Msg, Props]),
    {succeed, false, Props}.

succeed(Props, {move, Self, Source, Target}) when Self == self(), is_pid(Target) ->
    log("moved from ~p to ~p~n", [Source, Target]),
    erlmud_object:remove(Source, character, self()),
    erlmud_object:add(Target, character, self()),
    set(room, Target, Props);
succeed(Props, {move, Self, Source, Direction}) when Self == self(), is_atom(Direction) ->
    log("succeeded in moving ~p from ~p~n", [Direction, Source]),
    Props;
succeed(Props, {enter_world, Self})
    when Self == self() ->
    Room = proplists:get_value(room, Props),
    log("entering ~p~n", [Room]),
    erlmud_object:add(Room, character, self()),
    Props;
succeed(Props, {get, Self, Source, Item}) when Self == self() ->
    log("getting ~p from ~p~n\tProps: ~p~n", [Item, Source, Props]),
    Props;
%% I don't get this: we delete any current attack if a partially started
%% attack (no attack pid, just source and target) succeeds?
succeed(Props, {attack, Self, Target}) when Self == self() ->
    log("{attack, self(), ~p} succeeded~n"
        "Starting attack process~n"
        "\tProps: ~p~n",
        [Target, Props]),
    %attack(Target, stop_attack(Props));
    attack(Target, lists:keydelete(attack, 1, Props));
succeed(Props, {stop_attack, AttackPid}) ->
    log("Character ~p attack ~p stopped; remove (if applicable) from props:~n\t~p~n",
        [self(), AttackPid, Props]),
    lists:filter(fun({attack, Pid}) when Pid == AttackPid -> false; (_) -> true end, Props);
succeed(Props, {die, Self}) when Self == self() ->
    %% TODO: kill/disconnect all connected processes
    lists:keydelete(attack, 1, Props);
succeed(Props, {cleanup, Self}) when Self == self() ->
    %% TODO: drop all objects
    {stop, cleanup_succeeded, Props};
succeed(Props, Msg) ->
    log("saw ~p succeed with props~n~p~n", [Msg, Props]),
    Props.

fail(Props, target_is_dead, _Message) ->
    log("Stopping because target is dead~n", []),
    {stop, Props};
fail(Props, _Message, _Reason) ->
    Props.

attack(Target, Props) ->
    Args = [_Id = undefined,
            _Type = erlmud_attack,
            _Props = [{owner, self()}, {target, Target}]],
    {ok, Attack} = supervisor:start_child(erlmud_object_sup, Args),
    log("Attack ~p started, sending attempt~n", [Attack]),
    erlmud_object:attempt(Attack, {attack, Attack, self(), Target}),
    [{attack, Attack} | Props].

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
