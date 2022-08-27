%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(t1).

-export([go/0]).
-export([start/0]).
-export([tx/0]).

go() ->
    P1 = spawn(fun t1:start/0),
    P2 = spawn(fun t1:tx/0),
    P1 ! {pid, P2},
    P2 ! {pid, P1},
    {P1, P2}.

start() ->
  process_flag(trap_exit, true),
  rx().

rx() ->
  receive
    {pid, Pid} ->
      io:format("Linking pid ~p~n", [Pid]),
      link(Pid),
      rx();
    {msg, Msg} ->
      io:format("Message: ~p~n", [Msg]),
      rx();
    {'EXIT', Pid, Reason} ->
      io:format("Pid ~p died: ~p~n", [Pid, Reason]),
      rx()
    after 1000 ->
      io:format("Nothing~n"),
      rx()
  end.

tx() ->
  receive
    {pid, Pid} ->
      io:format("Sending to pid ~p~n", [Pid]),
      erlang:send_after(500, self(), {send, Pid}),
      tx();
    {send, Pid} ->
      Pid ! {msg, "Hi"},
      erlang:send_after(500, self(), {send, Pid}),
      tx()
  end.
