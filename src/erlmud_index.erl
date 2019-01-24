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

-module(erlmud_index).

-behaviour(gen_server).

-export([start_link/0]).
-export([put/2]).
-export([get/1]).
-export([get_pid/1]).
-export([get_id/1]).
-export([get_icon/1]).
-export([del/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(entry, {pid :: pid(),
                id :: atom(),
                icon :: atom()}).
-record(state, {index = [] :: list(#entry{})}).

%% api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(undefined, _) ->
    ok;
put(Pid, Prop = {Key, Value})
  when is_pid(Pid), is_atom(Key), is_atom(Value) ->
    gen_server:cast(erlmud_index, {put, Pid, Prop}).

get(Pid) when is_pid(Pid) ->
    gen_server:call(erlmud_index, {get, Pid}).

get_pid(Id) when is_atom(Id) ->
    gen_server:call(erlmud_index, {get_pid, Id}).

get_id(Pid) when is_pid(Pid) ->
    gen_server:call(erlmud_index, {get_id, Pid}).

get_icon(Pid) when is_pid(Pid) ->
    gen_server:call(erlmud_index, {get_icon, Pid}).

del(Pid) when is_pid(Pid) ->
    gen_server:cast(erlmud_index, {del, Pid}).

init([]) ->
    {ok, #state{}}.

handle_call({get, Pid}, _From, State) ->
    case lists:keyfind(Pid, 2, State#state.index) of
        false ->
            {reply, undefined, State};
        #entry{id = Id, icon = Icon} ->
            {reply, {Id, Icon}, State}
    end;
handle_call({get_pid, Id}, _From, State) ->
    case lists:keyfind(Id, 3, State#state.index) of
        false ->
            {reply, undefined, State};
        #entry{pid = Pid} ->
            {reply, Pid, State}
    end;
handle_call({get_id, Pid}, _From, State) ->
    case lists:keyfind(Pid, 2, State#state.index) of
        false ->
            {reply, undefined, State};
        #entry{id = Id} ->
            {reply, Id, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({put, Pid, {K, V}}, State = #state{index = Index}) ->
    Index2 =
    case lists:keyfind(Pid, 2, Index) of
        false ->
            [entry(Pid, K, V) | Index];
        Record ->
            Entry = update_entry(Record, K, V),
            lists:keyreplace(Pid, 2, Index, Entry)
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

entry(Pid, id, Id) ->
    #entry{pid = Pid, id = Id};
entry(Pid, icon, Icon) ->
    #entry{pid = Pid, icon = Icon}.

update_entry(Entry, id, Id) ->
    Entry#entry{id = Id};
update_entry(Entry, icon, Icon) ->
    Entry#entry{icon = Icon}.
