-module(erlmud_object).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {type :: atom(),
                state :: tuple()}).

%% API.

-spec start_link({atom(), tuple()}) -> {ok, pid()}.
start_link({Type, State}) ->
	gen_server:start_link(?MODULE, {Type, State}, []).

%% internal

union_first_rest(Old, New) ->
    All = sets:union(Old, New),
    First = hd(sets:to_list(All)),
    {First, sets:del_element(First, All)}.

%% gen_server.

init({Type, State}) ->
	{ok, #state{type = Type, state = State}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

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
