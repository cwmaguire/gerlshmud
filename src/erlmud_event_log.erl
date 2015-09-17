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
    case whereis(erlmud_event_log) of
        undefined ->
            exit("logger died");
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
                    ct:pal("Logger process is alive~n"),
                    ok;
                _ ->
                    exit("logger is dead")
            end
    end,
    gen_server:cast(erlmud_event_log,
                    {log, self(), To, Stage, Action, Params, Room, Next, Done, Subs}).

start_link() ->
    %_ = ets:new(?MODULE, [public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

init([]) ->
    process_flag(priority, max),
    LogPath = get_log_path(),
    {ok, LogFile} = file:open(LogPath ++ "/erlmud.log", [append]),
    {ok, HtmlFile} = file:open(LogPath ++ "/log.html", [append]),

    Line = lists:duplicate(80, $=),
    io:format(LogFile, "~n~n~s~n~p~n~s~n~n", [Line, os:timestamp(), Line]),

    io:format(HtmlFile,
              "<html>\n"
              "  <head>\n"
              "    <link rel=\"stylesheet\" href=\"log.css\">\n"
              "    <script src=\"lists.js\"></script>\n"
              "    <script src=\"test.js\"></script>\n"
              "    <script src=\"log.js\"></script>\n"
              "  </head>\n"
              "  <body>\n",
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
handle_cast(Args = {log, Level, Pid, Terms}, State) ->
    ct:pal("handle_cast: Log with Level, Pid, Terms:~n\t~p~n", [Args]),
    Self = io(self()),
    IoData = [Self, <<": ">>] ++ [[io(maybe_name(Term)), " "] || Term <- lists:flatten(Terms)],
    ct:pal("~p: IoData:~n~p~n", [?MODULE, IoData]),
    Props = erlmud_object:props(Pid),
    ct:pal("~p: Props:~n~p~n", [?MODULE, Props]),
    PropsWithNames = [{K, io(maybe_name(V))} || {K, V} <- Props],
    ct:pal("~p: Props:~n~p~n", [?MODULE, PropsWithNames]),
    ok = file:write(State#state.html_file,
                    spans(["log", Level, io(erlmud_index:get(Pid))],
                          [div_("log_message", IoData),
                           div_("log_props", io(PropsWithNames))])),
    {noreply, State};
handle_cast(Args = {log, From, To, Stage, Action, Params, Room, Next, Done, Subs}, State) ->
    ct:pal("handle_cast: Log with From, To, etc.~n\t~p~n", [Args]),
    FromName = erlmud_index:get(From),
    ct:pal("FromName: ~p~n", [FromName]),
    erlmud_dbg:add(erlmud_object),
    ct:pal("Added debug on erlmud_object~n", []),
    try
        FromProps = erlmud_object:props(From),
        ct:pal("FromProps: ~p~n", [FromProps]),
        FromPropsWithNames = [{K, maybe_name(V)} || {K, V} <- FromProps],
        ct:pal("FromPropsWithNames: ~p~n", [FromPropsWithNames])
    catch
        Error ->
            ct:pal("FromProps Error: ~p~n", [Error])
    end,
    dbg:stop_clear(),

    ToName = erlmud_index:get(To),
    ct:pal("ToName: ~p~n", [ToName]),
    ToProps = erlmud_object:props(To),
    ct:pal("ToProps: ~p~n", [ToProps]),
    ToPropsWithNames = [{K, maybe_name(V)} || {K, V} <- ToProps],

    ParamsWithNames = [{K, maybe_name(V)} || {K, V} <- Params],
    [RoomName] = names([Room]),
    NextNames = names(Next),
    DoneNames = names(Done),
    SubNames = names(Subs),

    ct:pal("Creating spans.~n", []),
    Spans = 
    %ok = file:write(State#state.html_file,
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
    ct:pal("Spans:~n\t~p~n", [[Spans]]),
    ok = file:write(State#state.html_file, [Spans]),
    %ok = file:write(State#state.html_file, ["</div>"]),
%),
    {noreply, State};
handle_cast(Msg, State) ->
    ct:pal("Unrecognized cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{html_file = HtmlFile}) ->
    ct:pal("Terminating erlmud_event_log: ~p~n", [Reason]),
    io:format(HtmlFile,
              "<script language=\"JavaScript\">"
              "createClassCheckboxes();"
              "</script>\n"
              "</body>\n</html>\n",
              []),
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

io(X) when is_binary(X) ->
    html_escape(io_lib:format("~s", [X]));
io(X) ->
    Io = case is_string(X) of
        true ->
            X;
        _ -> io_lib:format("~p", [X])
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
    ["<div class=\"", Class, "\">\n", Content, "</div>\n"].

spans(Classes, Content) ->
    lists:foldl(fun span/2, Content, lists:reverse(Classes)).

span(Class, Content) when not(is_list(Class)) ->
    span(io(Class), Content);
span(Class, Content) ->
    ["<span class=\"", Class, "\">\n", Content, "</span>\n"].

millis_as_list() ->
    integer_to_list(element(3, os:timestamp()) rem 1000).
