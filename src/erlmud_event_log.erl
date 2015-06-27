-module(erlmud_event_log).

-behaviour(gen_server).

-export([start_link/0]).
-export([log/2]).
-export([log/3]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {log_file :: file:io_device()}).

log(Msg, Params) ->
    gen_server:cast(erlmud_event_log, {log, self(), Msg, Params}).

log(From, To, Msg) ->
    gen_server:cast(erlmud_event_log, {log_msg, From, To, Msg}).

start_link() ->
    _ = ets:new(?MODULE, [public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    LogPath = get_log_path(),
    {ok, File} = file:open(LogPath ++ "/erlmud.log", [append]),
    Line = lists:duplicate(80, $=),
    io:format(File, "~n~n~s~n~p~n~s~n~n", [Line, os:timestamp(), Line]),
    {ok, #state{log_file = File}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({log, Pid, Msg, Params}, State) ->
    Id = erlmud_index:get(Pid),
    io:format(State#state.log_file, "~p (~p):~n" ++ Msg ++ "~n", [Pid, Id | Params]),
    {noreply, State};
handle_cast({log_msg, From, To, Msg}, State) ->
    FromId = erlmud_index:get(From),
    ToId = erlmud_index:get(To),
    io:format(State#state.log_file, "~p (~p) -> ~p (~p):~n~p~n~n", [From, FromId, To, ToId, Msg]),
    ets:insert(?MODULE, {os:timestamp(), {From, FromId}, {To, ToId}, Msg}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_log_path() ->
    case os:getenv("ERLMUD_LOG_PATH") of
        false ->
            file:get_cwd();
        Path ->
            Path
    end.
