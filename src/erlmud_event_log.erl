-module(erlmud_event_log).

-behaviour(gen_server).

-export([start_link/0]).
-export([log/2]).
-export([log/7]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {log_file :: file:io_device(),
                html_file :: file:io_device(),
                count :: integer()}).

log(Level, Terms) when is_atom(Level) ->
    gen_server:cast(erlmud_event_log, {log, Level, self(), Terms});
log(Msg, Params) ->
    gen_server:cast(erlmud_event_log, {old_log, self(), Msg, Params}).

log(To, Stage, Msg, Room, Next, Done, Subs) ->
    [Action | Params] = tuple_to_list(Msg),
    gen_server:cast(erlmud_event_log,
                    {log, self(), To, Stage, Action, Params, Room, Next, Done, Subs}).

start_link() ->
    _ = ets:new(?MODULE, [public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    LogPath = get_log_path(),
    {ok, LogFile} = file:open(LogPath ++ "/erlmud.log", [append]),
    {ok, HtmlFile} = file:open(LogPath ++ "/erlmud.html", [append]),

    Line = lists:duplicate(80, $=),
    io:format(LogFile, "~n~n~s~n~p~n~s~n~n", [Line, os:timestamp(), Line]),

    io:format(HtmlFile,
              "<html>"
                "<head>"
                  "<link rel=\"stylesheet\" href=\"log.css\">"
                "</head>"
                "<body>",
              []),
    {ok, #state{log_file = LogFile,
                html_file = HtmlFile}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-define(DIV(X), "<div>"X"</div>").
-define(SPAN(Class), "<span class=\"" Class "\">~p</span>").
-define(SPAN, ?SPAN("~p")).

handle_cast({old_log, Pid, Msg, Params}, State) ->
    Id = erlmud_index:get(Pid),
    io:format(State#state.log_file, "~p (~p):~n" ++ Msg ++ "~n",
              [Pid, Id | Params]),
    {noreply, State};
handle_cast({log, Level, Pid, Terms}, State) ->
    IoData = [io(maybe_name(Term)) || Term <- lists:flatten(Terms)],
    Props = erlmud_object:props(Pid),
    PropsWithNames = [{K, maybe_name(V)} || {K, V} <- Props],
    ok = file:write(State#state.log_file,
                    spans(["log", Level, io(erlmud_index:get(Pid))],
                          [div_("log_message", IoData),
                           div_("log_props", io(PropsWithNames))])),
    {noreply, State};
handle_cast({log, From, To, Stage, Action, Params, Room, Next, Done, Subs}, State) ->
    FromName = erlmud_index:get(From),
    FromProps = erlmud_object:props(From),
    FromPropsWithNames = [{K, maybe_name(V)} || {K, V} <- FromProps],

    ToName = erlmud_index:get(To),
    ToProps = erlmud_object:props(To),
    ToPropsWithNames = [{K, maybe_name(V)} || {K, V} <- ToProps],

    ParamsWithNames = [{K, maybe_name(V)} || {K, V} <- Params],
    [RoomName] = names([Room]),
    NextNames = names(Next),
    DoneNames = names(Done),
    SubNames = names(Subs),

    ok = file:write(State#state.log_file,
                    spans([Stage, Action, FromName, ToName],
                          [div_("columns",
                                [span("col_count", State#state.count),
                                 span("col_millies", millis_as_list()),
                                 span("col_from", From),
                                 span("col_from_name", FromName),
                                 span("col_to", To),
                                 span("col_to_name", ToName),
                                 span("col_stage", Stage),
                                 span("col_action", Action)]),
                           div_("params", io(ParamsWithNames)),
                           div_("from_props", io(FromPropsWithNames)),
                           div_("to_props", io(ToPropsWithNames)),
                           div_("rooms", io(RoomName)),
                           div_("next", io(NextNames)),
                           div_("done", io(DoneNames)),
                           div_("subs", io(SubNames))])),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{html_file = HtmlFile}) ->
    io:format(HtmlFile, "</body></html>", []),
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

millis_as_list() ->
    integer_to_list(element(3, os:timestemp()) rem 1000).
