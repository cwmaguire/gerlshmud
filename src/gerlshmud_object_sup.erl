%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_object_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% TODO check if I can use one_for_one and add child specs on the fly.
% That way they'd be restarted if they died.
init([]) ->
    Procs = [{object,
              {gerlshmud_object, start_link, []},
              transient,
              brutal_kill,
              worker,
              [gerlshmud_object]}],
    {ok, {{simple_one_for_one, 1, 5}, Procs}}.
