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

-export([start_link/1]).
-export([handle/2]).

-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

%% api

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_fsm:send_event(Pid, Msg).

% erlmud_object behaviours

added(_, _) -> ok.
removed(_, _) -> ok.

id(_Props, _Owner, Pid) ->
    "connection_" ++ "_" ++ Pid.

attempt(Player, Props, {send, Player, _Message}) ->
    {succed, true, Props};
attempt(Player, Props, {enter_world, Player}) ->
    {succeed, true, Props};
attempt(_OtherPlayer, Props, _Msg) ->
    {succeed, false, Props}.


succeed(Props, {send, _Player, Message}) ->
    log(debug, [<<"saw ">>, Message, <<" succeed with props\n">>]),
    %% Send the message to the connected socket
    Conn = prolists:get_value(conn, Props),
    Conn ! {send, Message},
    Props.

fail(Props, Reason, {enter_world, _Player}) ->
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    Props;
fail(Props, _Reason, _Message) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
