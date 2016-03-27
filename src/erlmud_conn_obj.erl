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
-module(erlmud_conn_obj).
-behaviour(erlmud_object).

-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).


% erlmud_object behaviours

added(_, _) -> ok.
removed(_, _) -> ok.

id(_Props, _Owner, Pid) ->
    "connection_" ++ "_" ++ Pid.

attempt(Player, Props, {send, Player, _Message}) ->
    {succeed, true, Props};
attempt(Player, Props, {move, Player, _TheVoid = undefined, _To, _NoExit = undefined}) ->
    %io:format("~p, Attempt move player into world~n", [?MODULE]),
    ct:pal("~p, see attempt to move player into world~n", [?MODULE]),
    {succeed, true, Props};
%attempt(Player, Props, {enter_world, Player}) ->
    %{succeed, true, Props};
attempt(_OtherPlayer, Props, _Msg) ->
    {succeed, false, Props}.


succeed(Props, {send, _Player, Message}) ->
    io:format("Saw send ~p ~p~n", [_Player, Message]),
    %log(debug, [<<"saw ">>, Message, <<" succeed with props\n">>]),
    %% Send the message to the connected socket
    Conn = prolists:get_value(conn, Props),
    Conn ! {send, Message},
    Props;
succeed(Props, {move, _Player, _TheVoid = undefined, _From, _NoExit = undefined}) ->
    io:format("Saw move player into world succeed~n", []),
    ct:pal("Saw move player into world succeed~n", []),
    %log(debug, [<<"Player ">>, Player, <<" succeed with props\n">>]),
    Props;
succeed(Props, Other) ->
    ct:pal("erlmud_conn_obj saw succeed with:~n\tProps: ~p~n\tMessage: ~p~n", [Props, Other]),
    Props.

fail(Props, Reason, {enter_world, _Player}) ->
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    Props;
fail(Props, Reason, {move, _Player, _From, _To, _Exit}) ->
    {Conn} = proplists:get_value(conn, Props),
    log(debug, [<<"failed to join starting room.">>]),
    Conn ! {disconnect, Reason},
    Props;
fail(Props, _Reason, _Message) ->
    ct:pal("erlmud_conn_obj saw fail with:~n\tProps: ~p~n\tMessage: ~p~n\t~p~n",
           [Props, _Message, _Reason]),
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
