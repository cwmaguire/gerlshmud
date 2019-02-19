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

-export([start_link/0]).
-export([put/2]).
-export([get/1]).
-export([get_pid/1]).
-export([get_id/1]).
-export([get_icon/1]).
-export([del/1]).
-export([update_pid/2]).
-export([update_prop/2]).
-export([subscribe_dead/2]).
-export([unsubscribe_dead/2]).
-export([replace_dead/2]).

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

put(undefined, _) ->
    ok;
put(Pid, Props) when is_pid(Pid), is_list(Props) ->
    gen_server:cast(?MODULE, {put, Pid, Props});

update_pid(Id, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {update_pid, Id, Pid});

update_prop(Pid, Prop = {Key, _})
  when is_pid(Pid), is_atom(Key) ->
    gen_server:cast(?MODULE, {update_prop, Pid, [Prop]}).

get(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {get, Pid}).

get_pid(Id) when is_atom(Id) ->
    gen_server:call(?MODULE, {get_pid, Id}).

get_id(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {get_id, Pid}).

get_icon(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {get_icon, Pid}).

del(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {del, Pid}).

subscribe_dead(Subscriber, DeadPid) ->
    gen_server:cast(?MODULE, {subscribe_dead, Subscriber, Pid}).

unsubscribe_dead(Subscriber, DeadPid) ->
    gen_server:cast(?MODULE, {unsubscribe_dead, Subscriber, Pid}).

replace_dead(OldPid, NewPid) ->
    gen_server:cast(?MODULE, {replace_pid, OldPid, NewPid}).

init([]) ->
    {ok, #state{}}.

handle_call({get, Pid}, _From, State) ->
    case fetch_object(Pid) of
        #object{id = Id, icon = Icon} ->
            {Id, Icon};
        _ ->
            undefined
    end.

    %case lists:keyfind(Pid, 2, State#state.index) of
    %    false ->
    %        {reply, undefined, State};
    %    #entry{id = Id, icon = Icon} ->
    %        {reply, {Id, Icon}, State}
    %end;
handle_call({get_pid, Id}, _From, State) ->
    Fun =
    fun() ->
        %MatchHead = #object{id=Id, pid='$1', _='_'},
        %Result = {'$1', '$2'},
        %mnesia:select(object, [{MathHead, [], [Result]}])
        mnesia:read(object, Id)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            {reply, undefined, State};
        {atomic, [Pid | _]} ->
            {reply, Pid, State}
    end

    %case lists:keyfind(Id, 3, State#state.index) of
    %    false ->
    %        {reply, undefined, State};
    %    #entry{pid = Pid} ->
    %        {reply, Pid, State}
    %end;
handle_call({get_id, Pid}, _From, State) ->
    case fetch_object(Pid) of
        #object{id = Id} ->
            Id;
        _ ->
            undefined
    end.
    %case lists:keyfind(Pid, 2, State#state.index) of
    %    false ->
    %        {reply, undefined, State};
    %    #entry{id = Id} ->
    %        {reply, Id, State}
    %end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({put, Pid, Props}) when is_pid(Pid), is_list(Props) ->
    Props2 = [pid2id(Prop) || Prop <- Props],
    Id = proplists:get_value(id, Props),
    Icon = proplists:get_value(icon, Props),
    Fun =
    fun() ->
        mnesia:write(#object{id = Id,
                             pid = self(),
                             icon = Icon,
                             properties = Props2})
    end,
    mnesia:transaction(Fun).

% TODO use index_read once we have an index on PID
handle_cast({put, Pid, {id, Id}}, State) ->

handle_cast({put, Pid, {icon, Icon}}, State) ->
    case fetch_object(Pid) of
        #object{id = Id, icon = Icon} ->
            {Id, Icon};
        _ ->
            undefined
    end.

    Index2 =
        case lists:keyfind(Pid, #entry.pid, Index) of
            false ->
                [entry(Pid, K, V) | Index];
            Record ->
                Entry = update_entry(Record, K, V),
                lists:keyreplace(Pid, #entry.pid, Index, Entry)
        end,
    {noreply, State};
handle_cast({update_pid, Id, Pid}, State = #state{index = Index}) ->
    Index2 =
        case lists:keyfind(Pid, #entry.id, Index) of
            false ->
                [entry(Pid, id, Id) | Index];
            Record ->
                Entry = update_entry(Record, pid, Pid),
                lists:keyreplace(Id, #entry.id, Index, Entry)
        end,
    {noreply, State#state{index = Index2}};
handle_cast({del, Pid}, State = #state{index = Index}) ->
    {noreply, State#state{index = lists:keydelete(Pid, 2, Index)}};
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
            MatchHead = #object{pid=Pid, id='$1', icon='$2', _='_'},
            Result = {'$1', '$2'},
            mnesia:select(object, [{MathHead, [], [Result]}])
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            {reply, undefined, State};
        {atomic, [Object | _]} ->
            {reply, Object, State}
    end.

update_object(Pid, {icon, Icon})
    Fun = fun(Object) ->
              Object#object{icon = Icon}
          end,
    update_object(Pid, Fun);
update_object(Pid, {id, Id})
    Fun = fun(Object) ->
              Object#object{id = Id}
          end,
    update_object(Pid, Fun),
update_object(Pid, UpdateFun) when is_function(UpdateFun) ->
    Fun =
        fun() ->
            MatchHead = #object{pid=Pid, _='_'},
            Result = {'$_'},
            case mnesia:select(object, [{MathHead, [], [Result]}]) of
                undefined
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            {reply, undefined, State};
        {atomic, [Object | _]} ->
            {reply, Object, State}
    end.

subscribe_dead(Subscriber, Pid) ->
    fun() ->
        case mnesia:read(replacement_pid, Pid) of
            [#replacement_pid{new_pid = NewPid} | _] ->
                Subsriber ! {replace_pid, Pid, NewPid};
            [] ->
                mnesia:write(#dead_pid_subscription{subscriber = self(),
                                                    dead_pid = Pid})
        end
    end,
    mnesia:transaction(Fun).

unsubscribe_dead(Subscriber, OldPid) ->
    OldRecord = #broken_link{object_pid = self(),
                             dead_pid = OldPid},
    fun() ->
        mnesia:delete(OldRecord)
    end,
    {atomic, ok} = mnesia:transaction(Fun).

replace_dead(OldPid, NewPid) ->
    %TODO fetch all subscriptions for OldPid and
    % notify the subscriber that there's a new PID

    %TODO insert new #replacement_pid record

    %TODO do a "send after" to delete the #replacement
    % after, say, a minute

ids2pids(Props) ->
    [id2pid(Prop) || Prop <- Props].

id2pid({K, MaybeId}) when is_atom(MaybeId) ->
    case gerlshmud_index:get_pid(MaybeId) of
        undefined ->
            {K, MaybeId};
        Pid when is_pid(Pid) ->
            {K, Pid}
    end;
id2pid({K, {MaybeId, BodyPart}})
  when is_atom(MaybeId), is_atom(BodyPart) ->
    case gerlshmud_index:get_pid(MaybeId) of
        undefined ->
            {K, {MaybeId, BodyPart}};
        Pid when is_pid(Pid) ->
            {K, {Pid, BodyPart}}
    end;
id2pid(Prop) ->
    Prop.


pid2id({K, {Pid, BodyPart}}) when is_pid(Pid) ->
    Id = gerlshmud_index:get_id(Pid),
    {K, {Id, BodyPart}};
pid2id({K, Pid}) when is_pid(Pid) ->
    Id = gerlshmud_index:get_id(Pid),
    {K, Id};
pid2id(Prop) ->
    Prop.

%entry(Pid, id, Id) ->
%    #entry{pid = Pid, id = Id};
%entry(Pid, icon, Icon) ->
%    #entry{pid = Pid, icon = Icon}.

%update_entry(Entry, pid, Pid) ->
%    Entry#entry{pid = Pid};
%update_entry(Entry, id, Id) ->
%    Entry#entry{id = Id};
%update_entry(Entry, icon, Icon) ->
%    Entry#entry{icon = Icon}.
