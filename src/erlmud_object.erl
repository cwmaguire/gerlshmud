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

-record(procs, {room = undefined :: pid(),
                done = [] :: [pid()],
                next = [] :: [pid()],
                subs = [] :: [pid()]}).

%% API.

-spec start_link(atom(), atom(), [{atom(), term()}]) -> {ok, pid()}.
start_link(Id, Type, Props) ->
	gen_server:start_link({local, Id}, ?MODULE, {Type, Props}, []).

populate(Pid, ProcIds) ->
    io:format("populate on ~p ...~n", [Pid]),
    gen_server:cast(Pid, {populate, ProcIds}).

%% gen_server.

init({Type, Props}) ->
	{ok, #state{type = Type, props = Props}}.

handle_call(props, _From, State) ->
    {reply, State#state.props, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({populate, ProcIds}, State = #state{props = Props}) ->
    {noreply, State#state{props = populate_(Props, ProcIds)}};
handle_cast({add, AddType, Pid}, State = #state{type = Type}) ->
    Props2 = Type:add(AddType, State#state.props, Pid),
    {noreply, State#state{props = Props2}};
handle_cast({remove, RemType, Pid}, State = #state{type = Type}) ->
    Props2 = Type:remove(RemType, Pid, State#state.props),
    {noreply, State#state{props = Props2}};
handle_cast({attempt, Msg, Procs}, State) ->
	{noreply, maybe_attempt(Msg, Procs, State)};
handle_cast(Fail = {fail, _, _}, State) ->
    {noreply, State#state{props = call(handle, Fail, State)}};
handle_cast(Success = {succeed, _}, State) ->
    {noreply, State#state{props = call(handle, Success, State)}}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

maybe_attempt(Msg,
              Procs = #procs{room = Room},
              State = #state{type = erlmud_exit, props = Props})
    when Room /= undefined->
    case erlmud_exit:is_attached_to_room(Props, Room) of
        true ->
            attempt(Msg, Procs, State);
        false ->
            handle(succeed, Msg, Procs)
    end;
maybe_attempt(Msg, Procs, State) ->
    attempt(Msg, Procs, State).

attempt(Msg, Procs, State = #state{type = Type, props = Props}) ->
    Results = {Result, _, Props2} = Type:handle(Props, {attempt, Msg}),
    handle(Result, Msg, merge(self(), Type, Results, Procs)),
    State#state{props = Props2}.

handle({resend, Target, Msg}, _OrigMsg, _NoProps) ->
    gen_server:cast(Target, {attempt, Msg, #procs{}});
handle({fail, Reason}, Msg, #procs{subs = Subs}) ->
    [gen_server:cast(Sub, {fail, Reason, Msg}) || Sub <- Subs];
handle(succeed, Msg, #procs{next = [], subs = Subs}) ->
    [gen_server:cast(Sub, {succeed, Msg}) || Sub <- Subs];
handle(succeed, Msg, Procs = #procs{subs = Subs}) ->
    case next(Procs) of
        {Next, Procs2} ->
            gen_server:cast(Next, {attempt, Msg, Procs2});
        [] ->
            [gen_server:cast(Sub, {succeed, Msg}) || Sub <- Subs]
    end.

procs(Props, undefined, _) ->
    [Pid || {_, Pid} <- Props, is_pid(Pid)].

populate_(Props, IdPids) ->
    [{K, proc(V, IdPids)} || {K, V} <- Props].

proc(Value, IdPids) when is_atom(Value) ->
    proplists:get_value(Value, IdPids, Value);
proc(Value, _) ->
    Value.

merge(_, _, {{resend, _, _}, _, _}, _) ->
    undefined;
merge(Self, erlmud_room, Results, Procs = #procs{room = undefined}) ->
    merge(Self, erlmud_room, Results, Procs#procs{room = Self});
merge(Self, Type, {_, Interested, Props}, Procs = #procs{}) ->
    merge_(Self,
           sub(Procs, Interested),
           procs(Props, Procs#procs.room, Type)).

merge_(Self, Procs = #procs{subs = Subs}, NewProcs) ->
    Done = ordsets:union(Procs#procs.done, [Self]),
    New = ordsets:subtract(ordsets:from_list(NewProcs), Done),
    Next = ordsets:union(Procs#procs.next, New),
    {Done, Next, Subs}.

sub(Procs = #procs{subs = Subs}, true) ->
    Procs#procs{subs = ordsets:union(Subs, [self()])};
sub(Procs, _) ->
    Procs.

next(#procs{done = Done, next = Next, subs = Subs}) ->
    NextProc = hd(ordsets:to_list(Next)),
    {NextProc, {Done, ordsets:del_element(NextProc, Next), Subs}}.

call(Fun, Arg, #state{type = Type, props = Props}) ->
    Type:Fun(Props, Arg).
