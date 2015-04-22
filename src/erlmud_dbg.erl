-module(erlmud_dbg).

-export([start/0]).
-export([add/1]).
-export([stop/0]).

start() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, [call]).

stop() ->
    dbg:stop_clear().

add(Module) ->
    add(Module, '_').

add(Module, Fun) ->
    add(Module, Fun, '_').

add(Module, Fun, Arity) ->
    dbg:tpl(Module, Fun, Arity, [{'_', [], [{return_trace}, {exception_trace}]}]).
