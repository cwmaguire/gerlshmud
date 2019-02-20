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
    gen_server:cast(?MODULE, {subscribe_dead, Subscriber, Pid}).

unsubscribe_dead(Subscriber, DeadPid) ->
    gen_server:cast(?MODULE, {unsubscribe_dead, Subscriber, Pid}).

replace_dead(OldPid, NewPid) ->
    gen_server:cast(?MODULE, {replace_pid, OldPid, NewPid}).

% gen_server

init([]) ->
    {ok, #state{}}.

handle_call({get, Pid}, _From, State) when is_pid(Pid) ->
    case fetch_object(Pid) of
        Object = #object{} ->
            Object;
        _ ->
            undefined
    end.
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

handle_cast({put, Props}) when is_list(Props) ->
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
    mnesia:transaction(Fun);

handle_cast({update_pid, Id, Pid}, State = #state{index = Index}) ->
    Fun =
        fun() ->
            [Object = #object{properties = Props}] = mnesia:read(object, Id),
            Props2 = [{pid, Pid} | lists:keydelete(Pid, 1, Props)],
            ok = mnesia:write(Object#object{pid = Pid, properties = Props2})
        end,
    {atomic, ok} = mnesia:transaction(Fun),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({delete, Object}) ->
    Fun =
        fun() ->
            mnesia:delete_object(Object)
        end,
    {atomic, ok} = mnesia:transaction(Fun);
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
            mnesia:select(object, [{MathHead, [], [Result]}])
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} ->
            {reply, undefined, State};
        {atomic, [Object = #object{properties = Props} | _]} ->
            Props2 = ids2pids(Props),
            {reply, Object#object{properties = Props}, State}
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
                OneMinute = 60 * 1000,
                Message = {delete, #dead_pid_subscription{subscriber = self(),
                                                          dead_pid = Pid}},
                erlang:send_after(OneMinute, self(), Message).
        end
    end,
    mnesia:transaction(Fun),

unsubscribe_dead(Subscriber, OldPid) ->
    OldRecord = #broken_link{object_pid = self(),
                             dead_pid = OldPid},
    fun() ->
        mnesia:delete(OldRecord)
    end,
    {atomic, ok} = mnesia:transaction(Fun).

replace_dead(OldPid, NewPid) ->
    Fun =
        fun() ->
            Object = #dead_pid_subscription{dead_pid = OldPid, _ = '_'},
            Subs = mnesia:match_object(Object)
            [Sub ! {replace_pid, OldPid, NewPid}
             || #dead_pid_subscription{subscriber = Sub} <- Subs].

            mnesia:write(#replacement_pid{old_pid = OldPid,
                                          new_pid = NewPid})
        end,
    {atomic, ok} = mnesia:transaction(Fun),

    OneMinute = 60 * 1000,
    Message = {delete, #replacement_pid{old_pid = OldPid, new_pid = NewPid}},
    erlang:send_after(OneMinute, self(), Message).

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

% Need to be able to compare previous PID
% to new PID in case of object death and
% resurrection
% Dead PIDs stay as PIDs so we can reset them
% when we know what the new PID is
pid2id(Prop = {pid, Pid}) when is_pid(Pid) ->
    Prop;
pid2id({K, {Pid, BodyPart}}) when is_pid(Pid) ->
    Id = gerlshmud_index:get_id(Pid),
    {K, {Id, BodyPart}};
pid2id({K, Pid}) when is_pid(Pid) ->
    Id = gerlshmud_index:get_id(Pid),
    {K, Id};
pid2id(Prop) ->
    Prop.
