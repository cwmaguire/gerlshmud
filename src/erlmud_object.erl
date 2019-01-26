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

-include("include/erlmud.hrl").

%% API.
-export([start_link/2]).
-export([populate/2]).
-export([attempt/2]).
-export([attempt/3]).
-export([attempt_after/3]).
-export([set/2]).
-export([props/1]).

%% Util
-export([has_pid/2]).
-export([value/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {props :: list(tuple())}).

-record(procs, {limit = undefined :: undefined | {atom(), integer(), atom()},
                room = undefined :: pid(),
                done = [] :: ordsets:ordset(pid()),
                next = [] :: ordsets:ordset(pid()),
                subs = [] :: ordsets:ordset(pid())}).

-callback added(atom(), pid()) -> ok.
-callback removed(atom(), pid()) -> ok.

%% API.

-spec start_link(any(), proplist()) -> {ok, pid()}.
start_link(Id, Props) ->
    %ct:pal("erlmud_obj:start_link(~p, ~p, ~p)~n", [Id, Type, Props]),
    {ok, Pid} = gen_server:start_link(?MODULE, Props, []),
    Id = id(Id, Pid, Props),
    erlmud_index:put(Pid, {id, Id}),
    Icon = proplists:get_value(icon, Props),
    erlmud_index:put(Pid, {icon, Icon}),
    {ok, Pid}.

id(_Id = undefined, Pid, Props) ->
    _PidString = pid_to_list(Pid)
                 ++ "_"
                 ++ binary_to_list(proplists:get_value(name, Props, <<"no_name">>));
id(Id, _, _) ->
    Id.

populate(Pid, ProcIds) ->
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

attempt_after(Millis, Pid, Msg) ->
    log([{stage, attempt_after},
         {object, self()},
         {target, Pid},
         {message, Msg},
         {millis, Millis}]),
    erlang:send_after(Millis, Pid, {Pid, Msg}).

set(Pid, Prop) ->
    send(Pid, {set, Prop}).

props(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, props);
        _ ->
            []
    end.

%% util

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

%% gen_server.

init(Props) ->
    process_flag(trap_exit, true),
    {ok, #state{props = Props}}.

handle_call(props, _From, State) ->
    {reply, State#state.props, State};
handle_call({get, Key}, _From, State = #state{props = Props}) ->
    {reply, proplists:get_all_values(Key, Props), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    handle_cast_(Msg, State).

handle_cast_({populate, ProcIds}, State = #state{props = Props}) ->
    log([{stage, none},
         {object, self()},
         {?EVENT, populate},
         {source, self()} |
         Props]),
    {noreply, State#state{props = populate_(Props, ProcIds)}};
handle_cast_({set, Prop = {K, _}}, State = #state{props = Props}) ->
    {noreply, State#state{props = lists:keystore(K, 1, Props, Prop)}};
handle_cast_({attempt, Msg, Procs}, State = #state{props = Props}) ->
    IsExit = proplists:get_value(is_exit, Props, false),
    {noreply, maybe_attempt(Msg, Procs, IsExit, State)};
handle_cast_({fail, Reason, Msg, LogProps}, State) ->
    case fail(Reason, Msg, State) of
        {stop, Props, LogProps} ->
            {_, ParentsList} = parents(Props),
            %% TODO: remove from index
            log([{stage, fail_stop},
                 {object, self()},
                 {owner, proplists:get_value(owner, Props)},
                 {message, Msg},
                 {stop_reason, Reason} |
                 Props ++ ParentsList ++ LogProps]),
            {stop, {shutdown, Reason}, State#state{props = Props}};
        {Props, LogProps} ->
            {_, ParentsList} = parents(Props),
            log([{stage, fail},
                 {object, self()},
                 {message, Msg},
                 {stop_reason, Reason} |
                 Props ++ ParentsList ++ LogProps]),
            {noreply, State#state{props = Props}}
    end;
handle_cast_({succeed, Msg}, State) ->
    case succeed(Msg, State) of
        {stop, Reason, Props, LogProps} ->
            {_, ParentsList} = parents(Props),
            log([{stage, succed_stop},
                 {object, self()},
                 {message, Msg},
                 {stop_reason, Reason} |
                 Props ++ ParentsList ++ LogProps]),
            {stop, {shutdown, Reason}, State#state{props = Props}};
        {Props, LogProps} ->
            {_, ParentsList} = parents(Props),
            log([{stage, succeed},
                 {object, self()},
                 {message, Msg} |
                 Props ++ ParentsList ++ LogProps]),
            {noreply, State#state{props = Props}}
    end.

handle_info({'EXIT', From, Reason}, State = #state{props = Props}) ->
    {_, ParentsList} = parents(Props),
    log([{?EVENT, exit},
         {object, self()},
         {source, From},
         {reason, Reason} |
         Props ++ ParentsList]),
    Props2 = lists:keydelete(From, 2, Props),
    {noreply, State#state{props = Props2}};
handle_info({Pid, Msg}, State) ->
    attempt(Pid, Msg),
    {noreply, State};
handle_info(Unknown, State = #state{props = Props}) ->
    {_, ParentsList} = parents(Props),
    log([{?EVENT, unknown_message},
         {object, self()},
         {message, Unknown} |
         Props ++ ParentsList]),
    {noreply, State}.

terminate(Reason, _State = #state{props = Props}) ->
    {_, ParentsList} = parents(Props),
    log([{?EVENT, shutdown},
         {object, self()},
         {reason, Reason} |
         Props ++ ParentsList]),
    erlmud_index:del(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

maybe_attempt(Msg,
              Procs = #procs{room = Room},
              _IsExit = true,
              State = #state{props = Props})
        when Room  /= undefined ->
    _ = case exit_has_room(Props, Room) of
            true ->
                attempt_(Msg, Procs, State);
            false ->
                _ = handle(succeed, Msg, done(self(), Procs), Props),
                State
        end;
maybe_attempt(Msg, Procs, _, State) ->
    attempt_(Msg, Procs, State).

exit_has_room(Props, Room) ->
    HasRoom = fun({{room, _}, R}) ->
                  R == Room;
                 (_) ->
                  false
              end,
    lists:any(HasRoom, Props).

attempt_(Msg,
         Procs,
         State = #state{props = Props}) ->
    {Parents, ParentsList} = parents(Props),
    %% So far it looks like nothing actually changes the object properties on attempt
    %% but I'm leaving it in for now
    %% I found a case: you attempt to shoot someone and you miss: the clip can lose a round ...
    %% except the clip could just listen for the result and decrement the ammunition then.
    %% I think I should stick with attempts never modifying the world ...
    %% if that's still possible, other than reserved resources ... although
    %% maybe even that should happen in succeed/fail
    {Handler,
     Results = {Result,
                Msg2,
                ShouldSubscribe,
                Props2,
                LogProps}}
      = ensure_log_props(
          ensure_message(Msg,
                         run_handlers({Parents, Props, Msg}))),
    log([{stage, attempt},
         {object, self()},
         {message, Msg},
         {handler, Handler},
         {subscribe, ShouldSubscribe} |
         Props2] ++
         ParentsList ++
         LogProps ++
         result_tuples(Result)),
    MergedProcs = merge(self(), is_room(Props), Results, Procs),
    _ = handle(Result, Msg2, MergedProcs, Props2),
    State#state{props = Props2}.

parents(Props) ->
    Owner = proplists:get_value(owner, Props),
    Character = proplists:get_value(character, Props),
    TopItem = proplists:get_value(top_item, Props),
    BodyPart = proplists:get_value(body_part, Props),
    Parents = #parents{owner = Owner,
                       character = Character,
                       top_item = TopItem,
                       body_part = BodyPart},
    {Parents,
     [{owner, Owner},
      {character, Character},
      {top_item, TopItem},
      {body_part, BodyPart}]}.

is_room(Props) ->
    proplists:get_value(is_room, Props, false).

result_tuples({fail, Reason}) when is_list(Reason) ->
    [{result, fail}, {reason, list_to_binary(Reason)}];
result_tuples({fail, Reason}) when is_atom(Reason) ->
    [{result, fail}, {reason, atom_to_binary(Reason, utf8)}];
result_tuples(Any = {fail, Any}) ->
    [{result, fail}, {reason, Any}];
result_tuples({resend, Target, Message}) ->
    [{result, resend}, {resend_to, Target}, {new_message, Message}];
result_tuples(succeed) ->
    [{result, succeed}];
result_tuples({broadcast, Message}) ->
    [{result, broadcast}, {new_message, Message}].

run_handlers(Attempt = {_, Props, _}) ->
    Handlers = proplists:get_value(handlers, Props),
    handle_attempt(Handlers, Attempt).

handle_attempt([], {_, Props, _}) ->
    _DefaultResponse = {no_handler, {succeed, false, Props}};
handle_attempt([Handler | Handlers], Attempt) ->
    %{_, Props, _} = Attempt,
    %Name = proplists:get_value(name, Props, "___"),
    %log([Name, self(), <<" running handler ">>, Handler]),
    case Handler:attempt(Attempt) of
        undefined ->
            handle_attempt(Handlers, Attempt);
        Result ->
            {Handler, Result}
    end.

ensure_message(Msg, {Handler, {Result, Sub, Props}})
  when is_atom(Sub), is_list(Props) ->
    {Handler, {Result, Msg, Sub, Props}};
ensure_message(Msg, {Handler, {Result, Sub, Props, Log}})
  when is_atom(Sub), is_list(Props), is_list(Log) ->
    {Handler, {Result, Msg, Sub, Props, Log}};
ensure_message(_, T) ->
    io:format(user, "T = ~p~n", [T]),
    T.

ensure_log_props({Handler, {Result, Msg, Sub, Props}})
  when is_atom(Sub), is_tuple(Msg), is_list(Props) ->
    {Handler, {Result, Msg, Sub, Props, []}};
ensure_log_props(WithLogProps) ->
    WithLogProps.


handle({resend, Target, Msg}, _OrigMsg, _NoProcs, _Props) ->
    send(Target, {attempt, Msg, #procs{}});
handle({fail, Reason}, Msg, Procs = #procs{subs = Subs}, _Props) ->
    [send(Sub, {fail, Reason, Msg}, Procs) || Sub <- Subs];
handle(succeed, Msg, Procs = #procs{subs = Subs}, _Props) ->
    _ = case next(Procs) of
        {Next, Procs2} ->
            send(Next, {attempt, Msg, Procs2});
        none ->
            [send(Sub, {succeed, Msg}, Procs) || Sub <- Subs]
    end;
handle({broadcast, Msg}, _Msg, _Procs, Props) ->
    %TODO have the handler that returned this also
    %return a filter; other handlers might not want
    %to only broadcast "down".
    NotParents = [Prop || Prop = {Key, _} <- Props,
                          Key /= owner,
                          Key /= character,
                          Key /= body_part,
                          Key /= top_item],
    [broadcast(V, Msg) || V <- procs(NotParents)].

broadcast(Pid, Msg) ->
    attempt(Pid, Msg).

send(Pid, SendMsg = {fail, _Reason, _Msg}, _Procs) ->
    send_(Pid, SendMsg);
send(Pid, SendMsg = {succeed, _Msg}, _Procs) ->
    send_(Pid, SendMsg).

send(Pid, SendMsg = {attempt, _Msg, _Procs}) ->
    send_(Pid, SendMsg);
send(Pid, Msg) ->
    send_(Pid, Msg).

send_(Pid, Msg) ->
    gen_server:cast(Pid, Msg).


populate_(Props, IdPids) ->
    {_, Props2} = lists:foldl(fun set_pid/2, {IdPids, []}, Props),
    Props2.

set_pid({K, {{pid, V1}, V2}}, {IdPids, Props}) ->
    {IdPids, [{K, {proc(V1, IdPids), V2}} | Props]};
set_pid({K, V}, {IdPids, Props}) ->
    {IdPids, [{K, proc(V, IdPids)} | Props]}.

proc(MaybeId, IdPids) when is_atom(MaybeId) ->
    MaybePid = proplists:get_value(MaybeId, IdPids, MaybeId),
    case is_pid(MaybePid) of
        true ->
            log([{?EVENT, link}, {target, MaybePid}]),
            link(MaybePid);
        false ->
            ok
    end,
    MaybePid;
proc(Value, _) ->
    Value.

procs(Props) ->
    lists:foldl(fun({_, Pid}, Acc) when is_pid(Pid) ->
                    [Pid | Acc];
                   ({_, Pids = [Pid | _]}, Acc) when is_pid(Pid) ->
                    Acc ++ Pids;
                   ({item, {ItemPid, Ref}}, Acc) when is_pid(ItemPid), is_reference(Ref) ->
                    [ItemPid | Acc];
                   (_NotPidProperty, Acc) ->
                    Acc
                end, [], Props).

merge(_, _, {{resend, _, _, _}, _, _, _, _}, _) ->
    undefined;
merge(_, _, {{broadcast, _}, _, _, _, _}, _) ->
    undefined;
merge(Self,
      IsRoom = true,
      Results,
      Procs = #procs{room = undefined}) ->
    merge(Self, IsRoom, Results, Procs#procs{room = Self});
merge(Self,
      _,
      {_,
       _,
       ShouldSubscribe,
       Props,
       _},
      Procs = #procs{}) ->
    merge_(Self,
           sub(Procs, ShouldSubscribe),
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
            %TODO ordsets are already lists, remove this. If anything, use from_list/1
            NextProc = hd(ordsets:to_list(Next)),
            {NextProc, Procs#procs{next = ordsets:del_element(NextProc, Next)}}
    end.

succeed(Message, #state{props = Props}) ->
    Handlers = proplists:get_value(handlers, Props),
    handle_success(Handlers, {Props, [], Message}).

handle_success(_NoMoreHandlers = [], {Props, LogProps, _Message}) ->
    {Props, LogProps};
handle_success([Handler | Handlers], {Props, LogProps, Message}) ->
    case Handler:succeed({Props, Message}) of
        {stop, Reason, Props2, LogProps2} ->
            MergedLogProps = merge_log_props(LogProps, LogProps2),
            {stop, Reason, Props2, MergedLogProps};
        {Props2, LogProps2} ->
            MergedLogProps = merge_log_props(LogProps, LogProps2),
            handle_success(Handlers, {Props2, MergedLogProps, Message});
        Props2 ->
            handle_success(Handlers, {Props2, LogProps, Message})
    end.

merge_log_props(Logs1, Logs2) ->
    lists:keymerge(1,
                   lists:keysort(1, Logs1),
                   lists:keysort(1, Logs2)).


fail(Reason, Message, #state{props = Props}) ->
    Handlers = proplists:get_value(handlers, Props),
    {Result, _, _} = lists:foldl(fun handle_fail/2, {Props, Reason, Message}, Handlers),
    Result.

handle_fail(_, Response = {{stop, _}, _, _}) ->
    Response;
handle_fail(HandlerModule, Failure = {_, Reason, Message}) ->
    Props = HandlerModule:fail(Failure),
    {Props, Reason, Message}.

value(Prop, Props, integer) ->
    prop(Prop, Props, fun is_integer/1, 0);
value(Prop, Props, boolean) ->
    prop(Prop, Props, fun is_boolean/1, false).

prop(Prop, Props, Fun, Default) ->
    Val = proplists:get_value(Prop, Props),
    case Fun(Val) of
        true ->
            Val;
        _ ->
            Default
    end.

log(Props0) ->
    Props = flatten(Props0),
    erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).

flatten(Props) ->
    Flattened = flatten(Props, []),
    lists:ukeysort(1, Flattened).

flatten([], List) ->
    List;
flatten([{K, [{K2, V} | L]} | Rest], Out) ->
    flatten([{K, L} | Rest], [{K2, V} | Out]);
flatten([T | Rest], Out) when is_tuple(T) ->
    flatten(Rest, [T | Out]).
