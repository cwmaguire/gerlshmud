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

-module(gerlshmud_index).

-behaviour(gen_server).

-include("include/gerlshmud.hrl").

-export([start_link/0]).
-export([put/1]).
-export([update_pid/2]).
-export([get/1]).
-export([ids2pids/1]).
-export([subscribe_dead/2]).
-export([unsubscribe_dead/2]).
-export([replace_dead/2]).
-export([del/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(entry, {pid :: pid(),
                id :: term(),
                icon :: atom()}).
-record(state, {index = [] :: list(#entry{})}).

%% api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Props) when is_list(Props) ->
    gen_server:cast(?MODULE, {put, Props}).

update_pid(Id, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {update_pid, Id, Pid}).

get(IdOrPid) ->
    gen_server:call(?MODULE, {get, IdOrPid}).

subscribe_dead(Subscriber, DeadPid) ->
    gen_server:cast(?MODULE, {subscribe_dead, Subscriber, DeadPid}).

unsubscribe_dead(Subscriber, DeadPid) ->
    gen_server:cast(?MODULE, {unsubscribe_dead, Subscriber, DeadPid}).

replace_dead(OldPid, NewPid) ->
    gen_server:cast(?MODULE, {replace_dead, OldPid, NewPid}).

del(Pid) ->
    gen_server:cast(?MODULE, {delete, Pid}).

% gen_server

init([]) ->
    {ok, #state{}}.

handle_call({get, Pid}, _From, State) when is_pid(Pid) ->
    Reply =
       case fetch_object(Pid) of
           Object = #object{} ->
               Object;
           _ ->
               undefined
       end,
    {reply, Reply, State};
handle_call({get, Id}, _From, State) ->
    Fun =
        fun() ->
            mnesia:read(object, Id)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            {reply, undefined, State};
        {atomic, [Object = #object{properties = Props} | _]} ->
            Props2 = ids2pids(Props),
            {reply, Object#object{properties = Props2}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({put, Props}, State) when is_list(Props) ->
    Props2 = [pid2id(Prop) || Prop <- Props],
    Pid = proplists:get_value(pid, Props),
    Id = proplists:get_value(id, Props),
    Icon = proplists:get_value(icon, Props),
    Fun =
    fun() ->
        mnesia:write(#object{id = Id,
                             pid = Pid,
                             icon = Icon,
                             properties = Props2})
    end,
    mnesia:transaction(Fun),
    {noreply, State};

handle_cast({update_pid, Id, Pid}, State) ->
    Fun =
        fun() ->
            [Object = #object{properties = Props}] = mnesia:read(object, Id),
            Props2 = [{pid, Pid} | lists:keydelete(Pid, 1, Props)],
            ok = mnesia:write(Object#object{pid = Pid, properties = Props2})
        end,
    {atomic, ok} = mnesia:transaction(Fun),
    {noreply, State};
handle_cast({subscribe_dead, Subscriber, DeadPid}, State) ->
    subscribe_dead_(Subscriber, DeadPid),
    {noreply, State};
handle_cast({unsubscribe_dead, Subscriber, OldPid}, State) ->
    unsubscribe_dead_(Subscriber, OldPid),
    {noreply, State};
handle_cast({replace_dead, OldPid, NewPid}, State) ->
    replace_dead_(OldPid, NewPid),
    {noreply, State};
handle_cast({delete, Pid}, State) ->
    Fun =
        fun() ->
            mnesia:delete({object, Pid}),
            mnesia:delete({dead_pid_subscription, Pid})
        end,
    {atomic, ok} = mnesia:transaction(Fun),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fetch_object(Pid) ->
    Fun =
        fun() ->
            MatchHead = #object{pid=Pid, _='_'},
            Result = '$_',
            mnesia:select(object, [{MatchHead, [], [Result]}])
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            undefined;
        {atomic, [Object = #object{properties = Props} | _]} ->
            Object#object{properties = ids2pids(Props)}
    end.

subscribe_dead_(Subscriber, Pid) ->
    Fun =
       fun() ->
           case mnesia:read(replacement_pid, Pid) of
               [#replacement_pid{new_pid = NewPid} | _] ->
                   Subscriber ! {replace_pid, Pid, NewPid};
               [] ->
                   mnesia:write(#dead_pid_subscription{subscriber = Subscriber,
                                                       dead_pid = Pid}),
                   OneMinute = 60 * 1000,
                   Message = {delete, #dead_pid_subscription{subscriber = Subscriber,
                                                             dead_pid = Pid}},
                   erlang:send_after(OneMinute, self(), Message)
           end
       end,
   {atomic, _Ref} = mnesia:transaction(Fun).

unsubscribe_dead_(Subscriber, OldPid) ->
    OldRecord = #dead_pid_subscription{subscriber = Subscriber,
                                       dead_pid = OldPid},
    Fun =
        fun() ->
            mnesia:delete_object(OldRecord)
        end,
    {atomic, ok} = mnesia:transaction(Fun).

replace_dead_(OldPid, NewPid) ->
    Fun =
        fun() ->
            Object = #dead_pid_subscription{dead_pid = OldPid, _ = '_'},
            Subs = mnesia:match_object(Object),
            [begin
                 Sub ! {replace_pid, OldPid, NewPid},
                 mnesia:delete_object(SubRec)
             end
             || SubRec = #dead_pid_subscription{subscriber = Sub} <- Subs],

             ok = mnesia:write(#replacement_pid{old_pid = OldPid,
                                                new_pid = NewPid})
        end,
    {atomic, ok} = mnesia:transaction(Fun),

    OneMinute = 60 * 1000,
    Message = {delete, #replacement_pid{old_pid = OldPid, new_pid = NewPid}},
    erlang:send_after(OneMinute, self(), Message).

ids2pids(Props) ->
    [id2pid(Prop) || Prop <- Props].

id2pid({K, MaybeId}) when is_atom(MaybeId) ->
    case get_pid(MaybeId) of
        Pid when is_pid(Pid) ->
            {K, Pid};
        _ ->
            {K, MaybeId}
    end;
id2pid({K, {MaybeId, BodyPart}})
  when is_atom(MaybeId), is_atom(BodyPart) ->
    case get_pid(MaybeId) of
        Pid when is_pid(Pid) ->
            {K, Pid};
        _ ->
            {K, {MaybeId, BodyPart}}
    end;
id2pid(Prop) ->
    Prop.

get_pid(MaybeId) ->
    Fun =
        fun() ->
            mnesia:read(object, MaybeId)
        end,
    case transaction(Fun) of
        [] ->
            undefined;
        [_Object = #object{pid = Pid} | _] ->
            Pid
    end.

transaction(Fun) ->
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

% Need to be able to compare previous PID
% to new PID in case of object death and
% resurrection
% Dead PIDs stay as PIDs so we can reset them
% when we know what the new PID is
pid2id(Prop = {pid, Pid}) when is_pid(Pid) ->
    Prop;
pid2id({K, {Pid, BodyPart}}) when is_pid(Pid) ->
    Id = get_id(Pid),
    {K, {Id, BodyPart}};
pid2id({K, Pid}) when is_pid(Pid) ->
    Id = get_id(Pid),
    {K, Id};
pid2id(Prop) ->
    Prop.

get_id(Pid) ->
    case fetch_object(Pid) of
        #object{id = Id} ->
            Id;
        _ ->
            undefined
    end.
