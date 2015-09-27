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
-export([props/1]).

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

-callback id(proplist(), list(), list()) -> list().
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
    erlmud_index:put(id(Id, Type, Props), Pid),
    {ok, Pid}.

id(_Id = undefined, Type, Props) ->
    Owner = case proplists:get_value(owner, Props, "NoOwner") of
                Pid when is_pid(Pid) ->
                    pid_to_list(Pid);
                Atom when is_atom(Atom) ->
                    atom_to_list(Atom);
                Binary when is_binary(Binary) ->
                    Binary;
                List when is_list(List) ->
                    error_logger:info_msg("Parent property of ~p is a list: ~p; all text should be binary.~n",
                           [self(), List]),
                    List
            end,
    PidString = pid_to_list(self()),
    Type:id(Props, Owner, PidString);
id(Id, _, _) ->
    Id.

populate(Pid, ProcIds) ->
    send(Pid, {populate, ProcIds}).

attempt(Pid, Msg) ->
    attempt(Pid, Msg, _ShouldSubscribe = true).

attempt(Pid, Msg, ShouldSubscribe) ->
    log([<<"attempt(Pid = ">>, Pid, <<", Msg = ">>, Msg, <<")">>]),
    Subs = case ShouldSubscribe of
               true ->
                   [self()];
               _ ->
                   []
           end,
    send(Pid, {attempt, Msg, #procs{subs = Subs}}).

attempt_after(Millis, Pid, Msg) ->
    log([<<"attempt after ">>, Millis, <<", Pid = ">>, Pid, <<": Msg = ">>, Msg]),
    erlang:send_after(Millis, Pid, {Pid, Msg}).

add(Pid, Type, AddPid) ->
    send(Pid, {add, Type, AddPid}).

remove(Pid, Type, RemovePid) ->
    send(Pid, {remove, Type, RemovePid}).

get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

set(Pid, Prop) ->
    send(Pid, {set, Prop}).

props(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, props);
        _ ->
            []
    end.

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

handle_cast(Msg, State) ->
    handle_cast_(Msg, State).

handle_cast_({populate, ProcIds}, State = #state{props = Props}) ->
    log([<<"populate on ">>, self()]),
    {noreply, State#state{props = populate_(Props, ProcIds)}};
handle_cast_({add, AddType, Pid}, State) ->
    Props2 = add_(AddType, State#state.props, Pid),
    (State#state.type):added(AddType, Pid),
    {noreply, State#state{props = Props2}};
handle_cast_({remove, RemType, Pid}, State) ->
    Props2 = remove_(RemType, Pid, State#state.props),
    (State#state.type):removed(RemType, Pid),
    {noreply, State#state{props = Props2}};
handle_cast_({set, Prop = {K, _}}, State = #state{props = Props}) ->
    {noreply, State#state{props = lists:keystore(K, 1, Props, Prop)}};
handle_cast_({attempt, Msg, Procs}, State) ->
    {noreply, maybe_attempt(Msg, Procs, State)};
handle_cast_({fail, Reason, Msg}, State) ->
    case fail(Reason, Msg, State) of
        {stop, Props} ->
            %% TODO: remove from index
            {stop, {shutdown, Reason}, State#state{props = Props}};
        Props ->
            {noreply, State#state{props = Props}}
    end;
handle_cast_({succeed, Msg}, State) ->
    case succeed(Msg, State) of
        {stop, Reason, Props} ->
            {stop, {shutdown, Reason}, State#state{props = Props}};
        Props ->
            {noreply, State#state{props = Props}}
     end.

handle_info({'EXIT', From, Reason}, State = #state{props = Props}) ->
    log([<<"handle_info EXIT Pid = ">>, From, <<", Reason = ">>, Reason, <<" Props: ">>, Props]),
    Props2 = lists:keydelete(From, 2, Props),
    log([<<"Props with dead pid removed: ">>, Props2]),
    {noreply, State#state{props = Props2}};
handle_info({Pid, Msg}, State) ->
    log([Pid, <<": handle_info attempt Msg = ">>, Msg]),
    attempt(Pid, Msg),
    {noreply, State};
handle_info(Unknown, State) ->
    log([<<"Unknown Message: ">>, Unknown]),
    {noreply, State}.

terminate(Reason, State) ->
    log([<<"erlmud_object ">>, self(), <<" shutting down Reason: ">>, Reason, <<" State: ">>, State]),
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
    Results = {Result, Msg2, ShouldSubscribe, Props2} = ensure_message(Msg, Type:attempt(Owner, Props, Msg)),
    log([Type, <<" ">>, self(), <<" ">>, Owner,
         <<"attempt: ">>, Msg, <<" -> ">>,
         ShouldSubscribe, <<", ">>, Result]),
    _ = handle(Result, Msg2, merge(self(), Type, Results, Procs)),
    State#state{props = Props2}.

ensure_message(Msg, {A, B, C}) ->
    {A, Msg, B, C};
ensure_message(_, T = {_, NewMsg, _, _}) ->
    log([<<"New message: ">>, NewMsg]),
    T.

handle({resend, Target, Msg}, OrigMsg, _NoProcs) ->
    log([<<"resending ">>, OrigMsg, <<" as ">>, Msg]),
    send(Target, {attempt, Msg, #procs{}});
handle({fail, Reason}, Msg, Procs = #procs{subs = Subs}) ->
    log([<<"failing msg: ">>, Msg, <<" with reaons: ">>, Reason, <<" subs: ">>, Subs]),
    [send(Sub, {fail, Reason, Msg}, Procs) || Sub <- Subs];
handle(succeed, Msg, Procs = #procs{subs = Subs}) ->
    _ = case next(Procs) of
        {Next, Procs2} ->
            send(Next, {attempt, Msg, Procs2});
        none ->
            [send(Sub, {succeed, Msg}, Procs) || Sub <- Subs]
    end.

send(Pid, SendMsg = {fail, _Reason, Msg}, Procs) ->
    log(Pid, fail, Msg, Procs),
    send_(Pid, SendMsg);
send(Pid, SendMsg = {succeed, Msg}, Procs) ->
    log(Pid, succeed, Msg, Procs),
    send_(Pid, SendMsg).

send(Pid, SendMsg = {attempt, Msg, Procs}) ->
    log(Pid, attempt, Msg, Procs),
    send_(Pid, SendMsg);
send(Pid, Msg) ->
    send_(Pid, Msg).

send_(Pid, Msg) ->
    gen_server:cast(Pid, Msg).


populate_(Props, IdPids) ->
    [{K, proc(V, IdPids)} || {K, V} <- Props].

proc(Value, IdPids) when is_atom(Value) ->
    Pid = proplists:get_value(Value, IdPids, Value),
    case is_pid(Pid) of
        true ->
            log([<<"Linking to pid: ">>, Pid, <<" for value ">>, Value]),
            link(Pid);
        false ->
            log([<<"Property value is not a pid: ">>, Value])
    end,
    Pid;
proc(Value, _) ->
    Value.

procs(Props) ->
    lists:foldl(fun procs/2, [], Props).

procs({_, Pid}, Pids) when is_pid(Pid) ->
    [Pid | Pids];
procs({_, MaybePids}, Pids) when is_list(MaybePids) ->
    Pids ++ [Pid || Pid <- MaybePids, is_pid(Pid)];
procs(_, Pids) ->
    Pids.

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
    log([<<"Props before removing ">>, RemType, <<" ">>, Obj, <<": ">>, Props]),
    NewProps = [Prop || Prop <- Props, Prop /= {RemType, Obj}],
    log([<<"Props after removing ">>, RemType, <<" ">>, Obj, <<": ">>, NewProps]),
    NewProps.

log(To, Stage, Msg, Procs) when is_tuple(Msg) ->
    erlmud_event_log:log(To,
                         Stage,
                         Msg,
                         Procs#procs.room,
                         Procs#procs.next,
                         Procs#procs.done,
                         Procs#procs.subs),
    erlang:yield().

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
