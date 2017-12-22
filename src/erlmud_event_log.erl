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

-define(DIV(X), "<div>"X"</div>").
-define(SPAN(Class), "<span class=\"" Class "\">~p</span>").
-define(SPAN, ?SPAN("~p")).

log(Level, Terms) when is_atom(Level) ->
    case whereis(?MODULE) of
        undefined ->
            %io:format("No ~p logger process found~nLevel: ~p~nTerms: ~p~n",
                      %[?MODULE, Level, Terms]);
            %exit("no logger process found");
            ok;
        _ ->
            ok
    end,
    gen_server:cast(?MODULE, {log, Level, self(), Terms});
log(Msg, Params) ->
    gen_server:cast(?MODULE, {old_log, self(), Msg, Params}).

log(To, Stage, Msg, Room, Next, Done, Subs) ->
    [Action | Params] = tuple_to_list(Msg),
    case whereis(erlmud_event_log) of
        undefined ->
            exit("logger not registered");
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                _ ->
                    exit("logger is dead")
            end
    end,
    gen_server:cast(erlmud_event_log,
                    {log, self(), To, Stage, Action, Params, Room, Next, Done, Subs}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(priority, max),
    io:format("Starting logger (~p)~n", [self()]),
    LogPath = get_log_path(),
    {ok, LogFile} = file:open(LogPath ++ "/erlmud.log", [append]),
    {ok, HtmlFile} = file:open(LogPath ++ "/log.html", [append]),

    Line = lists:duplicate(80, $=),
    io:format(LogFile, "~n~n~s~n~p~n~s~n~n", [Line, os:timestamp(), Line]),
    io:format(user, "Logger:~n\tLog file: ~p~n\tHTML File: ~p~n",
              [LogFile, HtmlFile]),

    {ok, #state{log_file = LogFile,
                html_file = HtmlFile}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({old_log, Pid, Msg, Params}, State) ->
    Id = erlmud_index:get(Pid),
    io:format(State#state.log_file, "~p (~p):~n" ++ Msg ++ "~n",
              [Pid, Id | Params]),
    {noreply, State};
handle_cast({log, Level, Pid, Terms}, State) ->
    try
    IoData = [[io(maybe_name(Term)), " "] || Term <- flatten(Terms)],
    %Props = erlmud_object:props(Pid),
    Props = props(Pid),
    %io:format("Props for Pid ~p:~n~p~n", [Pid, Props]),
    PropsWithNames = [{K, io(maybe_name(V))} || {K, V} <- Props],
    ok = file:write(State#state.html_file,
                    spans(["log", Level, io(erlmud_index:get(Pid))],
                          [div_("log_time", io(os:timestamp())),
                           div_("log_message", IoData),
                           div_("log_props", io([Pid, PropsWithNames]))]))
    catch
        Error ->
            ct:pal("~p caught error:~n\t~p~n", [?MODULE, Error])
    end,
    {noreply, State};
handle_cast({log, From, To, Stage, Action, _Params, _Room, _Next, _Done, _Subs}, State) ->
    FromName = erlmud_index:get(From),
    try
        FromProps = erlmud_object:props(From),
        _FromPropsWithNames = [{K, maybe_name(V)} || {K, V} <- FromProps]
    catch
        Error ->
            ct:pal("FromProps Error: ~p~n", [Error])
    end,

    ToName = erlmud_index:get(To),
    %ToProps = erlmud_object:props(To),
    %ToPropsWithNames = [{K, maybe_name(V)} || {K, V} <- ToProps],

    %ParamsWithNames = [{K, maybe_name(V)} || {K, V} <- Params],
    %[RoomName] = names([Room]),
    %NextNames = names(Next),
    %DoneNames = names(Done),
    %SubNames = names(Subs),

    try
    Spans =
                    spans([Stage, Action, FromName, ToName],
                          [div_("columns",
                                [%span("col_count", State#state.count)])
                                 span("col_millies", millis_as_list()),
                                 span("col_from", io(From))])
                                 %span("col_from_name", FromName),
                                 %span("col_to", To),
                                 %span("col_to_name", ToName),
                                 %span("col_stage", Stage),
                                 %span("col_action", Action)
                               % ])
                           %div_("params", io(ParamsWithNames)),
                           %div_("from_props", "_____")%,
                           %div_("from_props", io(FromPropsWithNames))%,
                           %div_("to_props", io(ToPropsWithNames)),
                           %div_("rooms", io(RoomName)),
                           %div_("next", io(NextNames)),
                           %div_("done", io(DoneNames))%,
                           %div_("subs", io(SubNames))
                          ]),
    %ct:pal("~p:spans(...) succeeded~n", [?MODULE]),
    ok = file:write(State#state.html_file, [Spans])
    catch
        Error2 ->
            ct:pal("spans(...) or file:write(...) error: ~p~n", [?MODULE, Error2])
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ct:pal("Unrecognized cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ct:pal("~p:handle_info(~p, State)~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{html_file = HtmlFile}) ->
    %ct:pal("Terminating erlmud_event_log: ~p~n", [Reason]),
    %io:format("Terminating erlmud_event_log: ~p~n", [Reason]),
    io:format(HtmlFile,
              "<script language=\"JavaScript\">"
              "createClassCheckboxes();"
              "</script>\n"
              "</body>\n</html>\n",
              []),
    ok;
terminate(Reason, State) ->
    ct:pal("Terminating erlmud_event_log: ~p~n", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

props(Pid) ->
    case erlmud_object:props(Pid) of
        undefined ->
            io:format(user, "Pid ~p has no props!", [Pid]),
            [];
        Props ->
            Props
    end.

get_log_path() ->
    case os:getenv("ERLMUD_LOG_PATH") of
        false ->
            {ok, CWD} = file:get_cwd(),
            CWD;
        Path ->
            Path
    end.

%names(Pids) ->
    %[erlmud_index:get(Pid) || Pid <- Pids].

maybe_name(Pid) when is_pid(Pid) ->
    {Pid, erlmud_index:get(Pid)};
maybe_name(NotPid) ->
    NotPid.

io(X) when is_binary(X) ->
    html_escape(io_lib:format("~s", [X]));
io(X) ->
    Io = case is_string(X) of
        true ->
            X;
        _ ->
            io_lib:format("~p", [X])
    end,
    html_escape(Io).

is_string([]) ->
    true;
is_string([X | Rest]) when is_integer(X),
                           X > 9, X < 127 ->
    is_string(Rest);
is_string(_) ->
    false.

html_escape(Io) when is_list(Io) ->
    lists:foldl(fun html_escape/2, "", lists:reverse(Io)).

html_escape(L, Acc) when is_list(L) ->
    html_escape(L) ++ Acc;
html_escape($>, Acc) -> [$&, $g, $t, $; | Acc];
html_escape($<, Acc) -> [$&, $l, $t, $; | Acc];
html_escape(X, Acc) -> [X | Acc].

div_(Class, Content) ->
    ["<div class=\"", Class, "\">\n", Content, "\n</div>\n"].

spans(Classes, Content) ->
    lists:foldl(fun span/2, Content, lists:reverse(Classes)).

span(Class, Content) when not(is_list(Class)) ->
    span(io(Class), Content);
span(Class, Content) ->
    ["<span class=\"", Class, "\">\n", Content, "</span>\n"].

millis_as_list() ->
    integer_to_list(element(3, os:timestamp()) rem 1000).

flatten(L) when is_list(L) ->
    lists:foldl(fun flatten/2, [], lists:reverse(L));
flatten(NotList) ->
    NotList.

flatten(T, Acc) when is_tuple(T) ->
    [H | TupleRest] = tuple_to_list(T),
    FlattenedTupleElem1 = [<<"{">>, flatten(H)],
    FlattenedTupleRest = lists:flatten([[<<",">>, flatten(X)] || X <- TupleRest]),
    FlattenedTupleElem1 ++ FlattenedTupleRest ++ [<<"} ">> | Acc];
flatten(L, Acc) when is_list(L) ->
    flatten(L) ++ Acc;
flatten(X, Acc) ->
    [X | Acc].
