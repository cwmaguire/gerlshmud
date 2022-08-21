-module(gerlshmud_dbg).

%-export([start/0]).
-export([add/1]).
-export([add/2]).
-export([add/3]).
-export([add_with_pid/2]).
-export([stop/0]).

%start() ->
    %dbg:start(),
    %dbg:tracer(),
    %dbg:p(all, [call]).

stop() ->
    dbg:stop_clear().

add(Module) ->
    add(Module, '_').

add(Module, Fun) ->
    add(Module, Fun, '_').

add(Module, Fun, Arity) ->
    maybe_start_tracer(all),
    dbg:tpl(Module, Fun, Arity, [{'_', [], [{return_trace}, {exception_trace}]}]).

add_with_pid(Pid, Module) ->
    maybe_start_tracer(Pid),
    dbg:tpl(Module, '_', '_', [{'_', [], [{return_trace}, {exception_trace}]}]).

maybe_start_tracer(Item) ->
    case dbg:tracer() of
        {error, already_started} ->
            ct:pal("Tracer already started~n", []),
            ok;
        _ ->
            {ok, List} = dbg:p(Item, call),
            ct:pal("Traced processes: ~p~n", [List])
    end.
