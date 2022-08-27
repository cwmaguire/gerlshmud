%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_app).
-behaviour(application).

-include("include/gerlshmud.hrl").

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

%% TODO call mnesia:wait_for_tables/2
setup_and_or_start_mnesia() ->
    case application:get_env(mnesia, dir) of
        undefined ->
            io:format("Mnesia dir not defined in mnesia application~n", []);
        {ok, Value} ->
            io:format("Mnesia dir ~p~n", [Value])
    end,
    case mnesia:system_info(use_dir) of
        true ->
            io:format("Mnesia schema already exists~n"),
            mnesia:start();
        false ->
            io:format("Mnesia schema doesn\'t exists~n"),
            setup_mnesia_schema()
    end,
    mnesia:wait_for_tables([object,
                            dead_pids_subscription,
                            replacement_pid],
                           2000).

setup_mnesia_schema() ->
    ok = mnesia:create_schema([node()]),
    io:format("Mnesia schema created~n"),
    ok = mnesia:start(),
    io:format("Mnesia started~n"),
    {atomic, ok} =
        mnesia:create_table(object,
                            [{attributes,
                              record_info(fields, object)}]),
    io:format("Mnesia table 'object' created~n"),
    % TODO add index on PID column so we can use index_read when
    % searching by PID
    {atomic, ok} =
        mnesia:create_table(dead_pid_subscription,
                            [{attributes,
                              record_info(fields, dead_pid_subscription)}]),
    io:format("Mnesia table 'dead_pid_subscription' created~n"),
    {atomic, ok} =
        mnesia:create_table(replacement_pid,
                            [{attributes,
                              record_info(fields, replacement_pid)}]),
    io:format("Mnesia table 'replacement_pid' created~n").
