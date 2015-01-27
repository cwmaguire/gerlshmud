-module(erlmud_object).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
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

-spec start_link({atom(), tuple()}) -> {ok, pid()}.
start_link({Type, State}) ->
	gen_server:start_link(?MODULE, {Type, State}, []).

populate(Pid, ProcIds) ->
    gen_server:cast(Pid, {populate, ProcIds}).

%% gen_server.

init({Type, Props}) ->
	{ok, #state{type = Type, obj_state = Type:create(Props)}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({populate, ProcIds}, State) ->
    {noreply, State#state{obj_state = populate_(State, ProcIds)}};
handle_cast({add, AddType, Pid}, State = #state{type = Type}) ->
    {noreply, Type:add(State, AddType, Pid)};
handle_cast({msg, Msg, Receivers, Subscribers}, State) ->
    Procs = (State#state.type):procs(State),
    {Receiver, NewReceivers} = union_first_rest(Receivers, Procs),
    Receiver ! {Msg, NewReceivers, Subscribers},
    io:format("Unintersting message ~p received.~n", Msg),
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

populate_(ObjState, IdPids) ->
    [Type | Fields] = tuple_to_list(ObjState),
    list_to_tuple([Type | [proc(Field, IdPids) || Field <- Fields]]).

proc(FieldObjs, IdPids) when is_list(FieldObjs) ->
    [Obj || FieldObj <- FieldObjs, Obj <- [proc(FieldObj, IdPids)], Obj /= undefined];
proc(FieldObj, IdPids) ->
    proplists:get_value(FieldObj, IdPids).

union_first_rest(Old, New) ->
    All = sets:union(Old, New),
    First = hd(sets:to_list(All)),
    {First, sets:del_element(First, All)}.
