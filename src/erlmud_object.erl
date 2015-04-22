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
-module(erlmud_object).
-behaviour(gen_server).

%% API.
-export([start_link/3]).
-export([populate/2]).
-export([attempt/2]).
-export([add/3]).
-export([remove/3]).

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

-type proplist() :: [{atom(), any()}].

-callback attempt(proplist(), tuple()) -> {boolean(), boolean(), proplist()}.
-callback succeed(proplist(), tuple()) -> proplist().
-callback fail(proplist(), string(), tuple()) -> proplist().
-callback added(atom(), pid()) -> ok.
-callback removed(atom(), pid()) -> ok.

%% API.

-spec start_link(atom(), atom(), proplist()) -> {ok, pid()}.
start_link(Id, Type, Props) ->
    {ok, Pid} = gen_server:start_link({local, Id}, ?MODULE, {Type, Props}, []),
    erlmud_index:put({Id, Pid}),
    {ok, Pid}.

populate(Pid, ProcIds) ->
    io:format("populate on ~p ...~n", [Pid]),
    gen_server:cast(Pid, {populate, ProcIds}).

attempt(Pid, Msg) ->
    Caller = self(),
    gen_server:cast(Pid, {attempt, Msg, #procs{subs = [Caller]}}).

add(Pid, Type, AddPid) ->
    gen_server:cast(Pid, {add, Type, AddPid}).

remove(Pid, Type, RemovePid) ->
    gen_server:cast(Pid, {remove, Type, RemovePid}).

%% gen_server.

init({Type, Props}) ->
    {ok, #state{type = Type, props = Props}}.

handle_call(props, _From, State) ->
    {reply, State#state.props, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({populate, ProcIds}, State = #state{props = Props}) ->
    {noreply, State#state{props = populate_(Props, ProcIds)}};
handle_cast({add, AddType, Pid}, State) ->
    Props2 = add_(AddType, State#state.props, Pid),
    (State#state.type):added(AddType, Pid),
    {noreply, State#state{props = Props2}};
handle_cast({remove, RemType, Pid}, State) ->
    Props2 = remove_(RemType, Pid, State#state.props),
    (State#state.type):removed(RemType, Pid),
    {noreply, State#state{props = Props2}};
handle_cast({attempt, Msg, Procs}, State) ->
    {noreply, maybe_attempt(Msg, Procs, State)};
handle_cast({fail, Reason, Msg}, State) ->
    {noreply, State#state{props = fail(Reason, Msg, State)}};
handle_cast({succeed, Msg}, State) ->
    {noreply, State#state{props = succeed(Msg, State)}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ct:pal("erlmud_object ~p shutting down~n", [self()]),
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
            attempt_(Msg, Procs, State);
        false ->
            handle(succeed, Msg, done(self, Procs)),
            State
    end;
maybe_attempt(Msg, Procs, State) ->
    attempt_(Msg, Procs, State).

attempt_(Msg, Procs, State = #state{type = Type, props = Props}) ->
    Results = {Result, _, Props2} = Type:attempt(Props, Msg),
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

populate_(Props, IdPids) ->
    [{K, proc(V, IdPids)} || {K, V} <- Props].

procs(Props) ->
    io:format("Object ~p is looking for pids in ~p~n", [self(), Props]),
    Pids = [Pid || {_, Pid} <- Props, is_pid(Pid)],
    io:format("Object ~p found pids: ~p~n", [self(), Pids]),
    Pids.

proc(Value, IdPids) when is_atom(Value) ->
    proplists:get_value(Value, IdPids, Value);
proc(Value, _) ->
    Value.

merge(_, _, {{resend, _, _}, _, _}, _) ->
    undefined;
merge(Self, erlmud_room, Results, Procs = #procs{room = undefined}) ->
    merge(Self, erlmud_room, Results, Procs#procs{room = Self});
merge(Self, _, {_, Interested, Props}, Procs = #procs{}) ->
    merge_(Self,
           sub(Procs, Interested),
           procs(Props)).

merge_(Self, Procs, NewProcs) ->
    Done = done(Self, Procs#procs.done),
    New = ordsets:subtract(ordsets:from_list(NewProcs), Done),
    Next = ordsets:union(Procs#procs.next, New),
    Procs#procs{done = Done, next = Next}.

done(Proc, Procs = #procs{done = Done}) ->
    Procs#procs{done = done(Proc, Done)};
done(Proc, Done) ->
    ordsets:union(Done, [Proc]).

sub(Procs = #procs{subs = Subs}, true) ->
    Procs#procs{subs = ordsets:union(Subs, [self()])};
sub(Procs, _) ->
    Procs.

next(Procs = #procs{next = Next}) ->
    NextProc = hd(ordsets:to_list(Next)),
    {NextProc, Procs#procs{next = ordsets:del_element(NextProc, Next)}}.

succeed(Message, #state{type = Type, props = Props}) ->
    Type:succeed(Props, Message).

fail(Reason, Message, #state{type = Type, props = Props}) ->
    Type:fail(Props, Reason, Message).

add_(Type, Props, Obj) ->
    case lists:member({Type, Obj}, Props) of
        false ->
            [{Type, Obj} | Props];
        true ->
            Props
    end.

remove_(RemType, Obj, Props) ->
    [Prop || Prop = {Type, Pid} <- Props, Type /= RemType, Pid /= Obj].
