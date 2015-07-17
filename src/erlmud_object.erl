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
-export([attempt/3]).
-export([attempt_after/3]).
-export([add/3]).
-export([remove/3]).
-export([get/2]).
-export([set/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {type :: atom(),
                props :: list(tuple())}).

-record(procs, {room = undefined :: undefined | pid(),
                done = [] :: ordsets:ordset(pid()),
                next = [] :: ordsets:ordset(pid()),
                subs = [] :: ordsets:ordset(pid())}).

-type proplist() :: [{atom(), any()}].
-type attempt() :: {atom(), Pid, Pid, Pid}.

-callback attempt(pid(), proplist(), tuple()) ->
    {succeed | {fail, atom()} | {resend, attempt()}, boolean(), proplist()}.
-callback succeed(proplist(), tuple()) -> proplist().
-callback fail(proplist(), string(), tuple()) -> proplist().
-callback added(atom(), pid()) -> ok.
-callback removed(atom(), pid()) -> ok.

%% API.

-spec start_link(any(), atom(), proplist()) -> {ok, pid()}.
start_link(Id, Type, Props) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Type, Props}, []),
    erlmud_index:put(Id, Pid),
    {ok, Pid}.

populate(Pid, ProcIds) ->
    log("populate on ~p~n", [Pid]),
    send(Pid, {populate, ProcIds}).

attempt(Pid, Msg) ->
    attempt(Pid, Msg, _ShouldSubscribe = true).

attempt(Pid, Msg, ShouldSubscribe) ->
    Subs = case ShouldSubscribe of
               true ->
                   [self()];
               _ ->
                   []
           end,
    send(Pid, {attempt, Msg, #procs{subs = Subs}}).

attempt_after(Milis, Pid, Msg) ->
    log("attempt after ~p, Pid = ~p~nMsg = ~p~n", [Milis, Pid, Msg]),
    erlang:send_after(Milis, Pid, {Pid, Msg}).

add(Pid, Type, AddPid) ->
    send(Pid, {add, Type, AddPid}).

remove(Pid, Type, RemovePid) ->
    send(Pid, {remove, Type, RemovePid}).

get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

set(Pid, Prop) ->
    send(Pid, {set, Prop}).


%% gen_server.

init({Type, Props}) ->
    process_flag(trap_exit, true),
    {ok, #state{type = Type, props = Props}}.

handle_call(props, _From, State) ->
    {reply, State#state.props, State};
handle_call({get, Key}, _From, State = #state{props = Props}) ->
    {reply, proplists:get_all_values(Key, Props), State};
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
handle_cast({set, Prop = {K, _}}, State = #state{props = Props}) ->
    {noreply, State#state{props = lists:keystore(K, 1, Props, Prop)}};
handle_cast({attempt, Msg, Procs}, State) ->
    {noreply, maybe_attempt(Msg, Procs, State)};
handle_cast({fail, Reason, Msg}, State) ->
    case fail(Reason, Msg, State) of
        {stop, Props} ->
            %% TODO: remove from index
            {stop, {shutdown, Reason}, State#state{props = Props}};
        Props ->
            {noreply, State#state{props = Props}}
    end;
handle_cast({succeed, Msg}, State) ->
    case succeed(Msg, State) of
        {stop, Reason, Props} ->
            {stop, {shutdown, Reason}, State#state{props = Props}};
        Props ->
            {noreply, State#state{props = Props}}
     end.

handle_info({'EXIT', From, Reason}, State = #state{props = Props}) ->
    log("handle_info EXIT Pid = ~p~nReason = ~p~nProps: ~p~n", [From, Reason, Props]),
    Props2 = lists:keydelete(From, 2, Props),
    log("Props with dead pid removed:~n~p~n", [Props2]),
    {noreply, State#state{props = Props2}};
handle_info({Pid, Msg}, State) ->
    log("handle_info attempt Pid = ~p~nMsg = ~p~n", [Pid, Msg]),
    attempt(Pid, Msg),
    {noreply, State};
handle_info(Unknown, State) ->
    log("Unknown Message: ~p~n", [Unknown]),
    {noreply, State}.

terminate(Reason, State) ->
    log("erlmud_object ~p shutting down~nReason: ~p~nState:~n\t~p~n",
           [self(), Reason, State]),
    erlmud_index:del(self()),
    ct:pal("erlmud_object ~p shutting down~nReason: ~p~nState:~n\t~p~n",
           [self(), Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

maybe_attempt(Msg,
              Procs = #procs{room = Room},
              State = #state{type = erlmud_exit, props = Props})
    when Room /= undefined->
    _ = case erlmud_exit:is_attached_to_room(Props, Room) of
        true ->
            attempt_(Msg, Procs, State);
        false ->
            _ = handle(succeed, Msg, done(self(), Procs)),
            State
    end;
maybe_attempt(Msg, Procs, State) ->
    attempt_(Msg, Procs, State).

attempt_(Msg,
         Procs,
         State = #state{type = Type,
                        props = Props}) ->
    Owner = proplists:get_value(owner, Props),
    Results = {Result, Msg2, _, Props2} = ensure_message(Msg, Type:attempt(Owner, Props, Msg)),
    _ = handle(Result, Msg2, merge(self(), Type, Results, Procs)),
    State#state{props = Props2}.

ensure_message(Msg, {A, B, C}) ->
    {A, Msg, B, C};
ensure_message(_, T) ->
    T.

handle({resend, Target, Msg}, OrigMsg, _NoProcs) ->
    log("resending~n\t~p~nas~n\t~p~n", [OrigMsg, Msg]),
    send(Target, {attempt, Msg, #procs{}});
handle({fail, Reason}, Msg, #procs{subs = Subs}) ->
    log("failing msg:~n~p~nwith reaons:~n~p~nsubs:~n~p~n", [Msg, Reason, Subs]),
    [send(Sub, {fail, Reason, Msg}) || Sub <- Subs];
handle(succeed, Msg, Procs = #procs{subs = Subs}) ->
    _ = case next(Procs) of
        {Next, Procs2} ->
            send(Next, {attempt, Msg, Procs2});
        none ->
            [send(Sub, {succeed, Msg}) || Sub <- Subs]
    end.

send(Pid, Msg) ->
    Id = erlmud_index:get(Pid),
    log("Sending:~nPid: ~p (~p)~nMsg: ~p~nPid is alive? ~p~n",
        [Pid, Id, Msg, is_process_alive(Pid)]),
    gen_server:cast(Pid, Msg).


populate_(Props, IdPids) ->
    [{K, proc(V, IdPids)} || {K, V} <- Props].

proc(Value, IdPids) when is_atom(Value) ->
    Pid = proplists:get_value(Value, IdPids, Value),
    case is_pid(Pid) of
        true ->
            log("Linking to pid: ~p for value ~p~n", [Pid, Value]),
            link(Pid);
        false ->
            log("Property value is not a pid: ~p~n", [Value])
    end,
    Pid;
proc(Value, _) ->
    Value.

procs(Props) ->
    [Pid || {_, Pid} <- Props, is_pid(Pid)].

merge(_, _, {{resend, _, _, _}, _, _, _}, _) ->
    undefined;
merge(Self, erlmud_room, Results, Procs = #procs{room = undefined}) ->
    merge(Self, erlmud_room, Results, Procs#procs{room = Self});
merge(Self, _, {_, _, Interested, Props}, Procs = #procs{}) ->
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

next(Procs = #procs{next = NextSet}) ->
    Next = ordsets:to_list(NextSet),
    case(Next) of
        [] ->
            none;
        _ ->
            NextProc = hd(ordsets:to_list(Next)),
            {NextProc, Procs#procs{next = ordsets:del_element(NextProc, Next)}}
    end.

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
    log("Props before removing ~p ~p: ~p~n", [RemType, Obj, Props]),
    NewProps = [Prop || Prop <- Props, Prop /= {RemType, Obj}],
    log("Props after removing ~p ~p: ~p~n", [RemType, Obj, NewProps]),
    NewProps.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
