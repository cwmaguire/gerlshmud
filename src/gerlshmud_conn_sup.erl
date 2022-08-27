%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_conn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{conn_fsm,
              {gerlshmud_conn, start_link, []},
              transient,
              brutal_kill,
              worker,
              [gerlshmud_conn]}],
    {ok, {{simple_one_for_one, 1, 5}, Procs}}.
