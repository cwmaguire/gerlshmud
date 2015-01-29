-module(erlmud_object).
-behaviour(gen_server).

%% API.
-export([start_link/3]).
-export([populate/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {type :: atom(),
                props :: tuple()}).

%% API.

-spec start_link(atom(), atom(), [{atom(), term()}]) -> {ok, pid()}.
start_link(Id, Type, Props) ->
	gen_server:start_link({local, Id}, ?MODULE, {Type, Props}, []).

populate(Pid, ProcIds) ->
    io:format("populate on ~p ...~n", [Pid]),
    gen_server:cast(Pid, {populate, ProcIds}).

%% gen_server.

init({Type, Props}) ->
	{ok, #state{type = Type, props = Type:create(Props)}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({populate, ProcIds}, State = #state{props = Props}) ->
    {noreply, State#state{props = populate_(Props, ProcIds)}};
handle_cast({add, AddType, Pid}, State = #state{type = Type}) ->
    Props2 = Type:add(State#state.props, AddType, Pid),
    {noreply, State#state{props = Props2}};
handle_cast({attempt, Msg, Procs},
            State = #state{type = Type, props = Props}) ->
    {Result, Interested, Props2} = Type:handle(Props, {attempt, Msg}),
    Procs2 = merge(sub(Procs, Interested), call(procs, State)),
    State2 = State#state{props = Props2},
    handle(Result, Msg, Procs2, State2),
	{noreply, State};
handle_cast(Fail = {fail, _, _}, State) ->
    State2 = call(handle, Fail, State),
    {noreply, State2};
handle_cast(Success = {succeed, _}, State) ->
    io:format("~p ~p handling ~p~n", [State#state.type, self(), Success]),
    State2 = call(handle, Success, State),
    {noreply, State2}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

handle({fail, Reason}, Msg, {_, _, Subs}, _) ->
    [gen_server:cast(Sub, {fail, Reason, Msg}) || Sub <- Subs];
handle(succeed, Msg, {_, [], Subs}, #state{type = Type}) ->
    io:format("~p ~p: handling succeed, ~p, [], ~p, ~p~n",
              [Type, self(), Msg, Subs, no_state]),
    [gen_server:cast(Sub, {succeed, Msg}) || Sub <- Subs];
handle(succeed, Msg, Procs = {_, _, Subs}, State = #state{type = Type}) ->
    io:format("~p ~p handling succeed, ~p, ~p, ~p~n",
              [Type, self(), Msg, Procs, State]),
    case next(Procs) of
        {Next, Procs2} ->
            gen_server:cast(Next, {attempt, Msg, Procs2});
        [] ->
            [gen_server:cast(Sub, {succeed, Msg}) || Sub <- Subs]
    end.

populate_(Props, IdPids) ->
    [proc(K, V, IdPids) || {K, V} <- Props].

proc(K, Vs, IdPids) when is_list(Vs) ->
    {K, [proc(V, IdPids) || V <- Vs]};
proc(K, Value, IdPids) ->
    {K, proc(Value, IdPids)}.

proc({K, Value}, IdPids) when is_list(Value) ->
    {K, proplists:get_value(Value, IdPids, Value)};
proc(Value, IdPids) when is_atom(Value) ->
    proplists:get_value(Value, IdPids, Value);
proc(Value, _) ->
    Value.

sub({Old, New, Subs}, true) ->
    {Old, New, [self() | Subs]};
sub(Procs, _) ->
    Procs.

merge({Old, New, Subs}, Procs) ->
    New2 = ordsets:union(New, ordsets:subtract(ordsets:from_list(Procs), Old)),
    {Old, New2, Subs}.

next({Old, New, Subs}) ->
    Next = hd(ordsets:to_list(New)),
    {Next, {Old, ordsets:del_element(Next, New), Subs}}.

call(Fun, #state{type = Type, props = Props}) ->
    Type:Fun(Props).

call(Fun, Arg, #state{type = Type, props = Props}) ->
    Type:Fun(Props, Arg).
