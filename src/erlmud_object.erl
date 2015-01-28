-module(erlmud_object).
-behaviour(gen_server).

%% API.
-export([start_link/2]).
-export([populate/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {type :: atom(),
                obj_state :: tuple()}).

%% API.

-spec start_link(atom(), [{atom(), term()}]) -> {ok, pid()}.
start_link(Type, Props) ->
	gen_server:start_link(?MODULE, {Type, Props}, []).

populate(Pid, ProcIds) ->
    io:format("populate on ~p ...~n", [Pid]),
    gen_server:cast(Pid, {populate, ProcIds}).

%% gen_server.

init({Type, Props}) ->
	{ok, #state{type = Type, obj_state = Type:create(Props)}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({populate, ProcIds}, State = #state{obj_state = Props}) ->
    {noreply, State#state{obj_state = populate_(Props, ProcIds)}};
handle_cast({add, AddType, Pid}, State = #state{type = Type}) ->
    {noreply, Type:add(State, AddType, Pid)};
handle_cast({attempt, Msg, Objs, Subs}, State = #state{type = Type}) ->
    {Result, Interested, State2} = Type:handle(Msg, State),
    Subs2 = case Interested of
                    true ->
                        [self() | Subs];
                    _ ->
                        Subs
                end,
    handle(Result, Msg, Objs, Subs2, State2),
	{noreply, State2};
handle_cast(Fail = {fail, _, _}, State) ->
    State2 = call(handle, Fail, State),
    {noreply, State2};
handle_cast(Success = {succeed, _}, State) ->
    State2 = call(handle, Success, State),
    {noreply, State2}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

handle({fail, Reason}, Msg, _, Subs, _) ->
    [gen_server:cast(Sub, {fail, Reason, Msg}) || Sub <- Subs];
handle(succeed, Msg, _NoMoreTargets = [], Subs, _) ->
    [gen_server:cast(Sub, {succeed, Msg}) || Sub <- Subs];
handle(succeed, Msg, [_ | _] = Targets, Subs, State) ->
    ConnectedObjs = call(procs, State),
    {Target, Targets2} = union_first_rest(Targets, ConnectedObjs),
    gen_server:cast(Target, {msg, Msg, Targets2, Subs}).

populate_(Props, IdPids) ->
    [proc(K, V, IdPids) || {K, V} <- Props].

proc(K, Vs, IdPids) when is_list(Vs) ->
    {K, [proc(V, IdPids) || V <- Vs]};
proc(K, Value, IdPids) ->
    {K, proc(Value, IdPids)}.

proc(Value, IdPids) when is_atom(Value) ->
    proplists:get_value(Value, IdPids, Value);
proc(Value, _) ->
    Value.

union_first_rest(Old, New) ->
    All = sets:union(Old, New),
    First = hd(sets:to_list(All)),
    {First, sets:del_element(First, All)}.

call(Fun, #state{type = Type, obj_state = ObjState}) ->
    Type:Fun(ObjState).

call(Fun, Arg, #state{type = Type, obj_state = ObjState}) ->
    Type:Fun(ObjState, Arg).
