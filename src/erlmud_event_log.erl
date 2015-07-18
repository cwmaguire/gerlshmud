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

-record(state, {log_file :: file:io_device(),
                count :: integer()}).

log(Level, IoData) ->
    gen_server:cast(erlmud_event_log, {Level, self(), IoData}).

%log(Msg, Params) ->
    %gen_server:cast(erlmud_event_log, {log, self(), Msg, Params}).

log(From, To, Msg) ->
    gen_server:cast(erlmud_event_log, {log_msg, From, To, Msg}).

start_link() ->
    _ = ets:new(?MODULE, [public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    LogPath = get_log_path(),
    {ok, File} = file:open(LogPath ++ "/erlmud.log", [append]),
    %Line = lists:duplicate(80, $=),
    %io:format(File, "~n~n~s~n~p~n~s~n~n", [Line, os:timestamp(), Line]),
    io:format(File,
              "<html>"
                "<head>"
                  "<link rel=\"stylesheet\" href=\"log.css\">"
                "</head>"
                "<body>",
              []),
    {ok, #state{log_file = File}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-define(DIV(X), "<div>"X"</div>").
-define(SPAN(Class), "<span class=\"" Class "\">~p</span>").
-define(SPAN, ?SPAN("~p")).

%% Pid, Name, Module, Time, Type (Attempt, Succeed, Fail, Link, etc.), Message
handle_cast({log, From, To, Props, Stage, {Action, Params}, Room, Next, Done, Subs}, State) ->
    FromName = erlmud_index:get(From),
    ToName = erlmud_index:get(To),
    PropsWithNames = [{K, maybe_name(V)} || {K, V} <- Props],
    ParamsWithNames = [{K, maybe_name(V)} || {K, V} <- Params],
    [RoomName] = names([Room]),
    NextNames = names(Next),
    DoneNames = names(Done),
    SubNames = names(Subs),

    {_, _, Micros} = os:timestamp(),
    Millis = Micros rem 1000,
    file:write(State#state.log_file,
               spans([Stage, Action, FromName, ToName],
                     [div_("columns",
                           [span("col_count", State#state.count),
                            span("col_millies", Millis),
                            span("col_from", From),
                            span("col_from_name", FromName),
                            span("col_to", To),
                            span("col_to_name", ToName),
                            span("col_stage", Stage),
                            span("col_action", Action)]),
                      div_("params", io(ParamsWithNames)),
                      div_("props", io(PropsWithNames)),
                      div_("rooms", io(RoomName)),
                      div_("next", io(NextNames)),
                      div_("done", io(DoneNames)),
                      div_("subs", io(SubNames))])),
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

terminate(_Reason, #state{log_file = File}) ->
    io:format(File, "</body></html>", []),
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

names(Pids) ->
    [erlmud_index:get(Pid) || Pid <- Pids].

maybe_name(Pid) when is_pid(Pid) ->
    erlmud_index:get(Pid);
maybe_name(NotPid) ->
    NotPid.

io(X) ->
    io_lib:format("~p", [X]).

div_(Class, Content) ->
    ["<div class=\"", Class, "\">", Content, "</div>"].

spans(Classes, Content) ->
    lists:foldl(fun span/2, Content, lists:reverse(Classes)).

span(Class, Content) when not(is_list(Class)) ->
    span(io(Class), Content);
span(Class, Content) ->
    ["<span class=\"", Class, "\">", Content, "</span>"].
