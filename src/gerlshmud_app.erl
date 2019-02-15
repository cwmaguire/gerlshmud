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
-module(gerlshmud_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_Type, _Args) ->
    setup_and_or_start_mnesia(),
    Port = case application:get_env(gerlshmud, port) of
               {ok, EnvPort} ->
                   EnvPort;
               _ ->
                   8080
           end,

    Paths = [{"/", gerlshmud_websocket, ?NO_OPTIONS},
             {"/log", gerlshmud_websocket_log, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, gerlshmud, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_http(gerlshmud_http_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    gerlshmud_sup:start_link().

stop(_State) ->
	ok.

setup_and_or_start_mnesia() ->
    case mnesia:system_info(use_dir) of
        true ->
            mnesia:start();
        false ->
            setup_mnesia_schema()
    end.

setup_mnesia_schema() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} =
    mnesia:create_table(object,
                        [{attributes, record_info(fields, employee)}]).
