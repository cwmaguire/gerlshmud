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
-module(gerlshmud_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{object_sup,
              {gerlshmud_object_sup, start_link, []},
              permanent,
              brutal_kill,
              supervisor,
              [gerlshmud_object_sup]},
             {conn_sup,
              {gerlshmud_conn_sup, start_link, []},
              permanent,
              brutal_kill,
              supervisor,
              [gerlshmud_conn_sup]},
             {gerlshmud_index,
              {gerlshmud_index, start_link, []},
              permanent,
              brutal_kill,
              worker,
              [gerlshmud_index]},
             {gerlshmud_event_log,
              {gerlshmud_event_log, start_link, []},
              permanent,
              brutal_kill,
              worker,
              [gerlshmud_event_log]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
