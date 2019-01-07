-module(erlmud_event_log).

-behaviour(gen_server).

-export([start_link/0]).
-export([log/2]).
-export([log/3]).
-export([register/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {log_file :: file:io_device(),
                html_file :: file:io_device(),
                processes = [] :: [pid()]}).

-define(DIV(X), "<div>"X"</div>").
-define(SPAN(Class), "<span class=\"" Class "\">~p</span>").
-define(SPAN, ?SPAN("~p")).

log(Level, Terms) when is_atom(Level) ->
    Self = self(),
    log(Self, Level, Terms).

log(Pid, Level, Terms) when is_atom(Level) ->
    case whereis(?MODULE) of
        undefined ->
            io:format(user,
                      "erlmud_event_log process not found~n"
                      "Pid: ~p, Level: ~p, Terms: ~p~n",
                      [Pid, Level, Terms]);
        _ ->
            gen_server:cast(?MODULE, {log, Pid, Level, Terms})
    end.

register(Logger) when is_function(Logger) ->
    gen_server:cast(?MODULE, {register, Logger}).

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

handle_call(Request, From, State) ->
    io:format(user, "erlmud_event_log:handle_call(~p, ~p, ~p)~n",
              [Request, From, State]),
    {reply, ignored, State}.

handle_cast({log, Pid, Level, Terms}, State) ->
    try
        LevelBin = list_to_binary(atom_to_list(Level)),
        Data = flatten(Terms),
        ok = file:write(State#state.log_file, <<Data/binary, "\n">>),

        HTMLSafe = html_escape(flatten(Terms)),
        Props = props(Pid),
        PropsWithNames = [{K, <<": ">>, maybe_name(V)} || {K, V} <- Props],
        PropsBin = flatten(lists:join(<<", ">>, PropsWithNames)),
        ok = file:write(State#state.html_file,
                        spans([<<"log">>, LevelBin, p2b(Pid)],
                              [div_(<<"log_time">>, ts2b(os:timestamp())),
                               div_(<<"log_message">>, HTMLSafe),
                               div_(<<"log_props">>, [Pid, PropsBin])]))
    catch
        Error ->
            io:format(user, "~p caught error:~n\t~p~n", [?MODULE, Error])
    end,
    {noreply, State};

handle_case({register, Logger}, State = #state{loggers = Loggers}) ->
    {noreply, State#state{processes = [Logger | Loggers]}}.

handle_cast(Msg, State) ->
    io:format(user, "Unrecognized cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format(user, "~p:handle_info(~p, State)~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{html_file = HtmlFile}) ->
    io:format(HtmlFile,
              "<script language=\"JavaScript\">"
              "createClassCheckboxes();"
              "</script>\n"
              "</body>\n</html>\n",
              []),
    ok;
terminate(Reason, State) ->
    io:format(user, "Terminating erlmud_event_log: ~p~n~p~n", [Reason, State]).

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

maybe_name(Pid) when is_pid(Pid) ->
    {Pid, erlmud_index:get(Pid)};
maybe_name(NotPid) ->
    NotPid.

html_escape(List) when is_list(List) ->
    html_escape(list_to_binary(List));
html_escape(Bin) when is_binary(Bin) ->
    Replacements = [{<<"<">>, <<"&lt;">>},{<<">">>, <<"&gt;">>}],
    lists:foldl(fun html_escape/2, Bin, Replacements).

html_escape({Pattern, Replace}, Acc) ->
    binary:replace(Acc, Pattern, Replace, [global]).

div_(Class, Content) when is_binary(Class) ->
    ContentBin = flatten(Content),
    %ct:pal("~p: ContentBin~n\t~p~n", [?MODULE, ContentBin]),
    <<"<div class=\"", Class/binary, "\">\n", ContentBin/binary, "\n</div>\n">>.

spans(Classes, Content) ->
    ContentBin = flatten(Content),
    lists:foldl(fun span/2, ContentBin, lists:reverse(Classes)).

span(Class, Content) when is_binary(Class) ->
    <<"<span class=\"", Class/binary, "\">\n", Content/binary, "</span>\n">>.

flatten(L) when is_list(L) ->
    lists:foldl(fun flatten/2, <<>>, L);
flatten(NotList) ->
    NotList.

flatten(Bin, Acc) when is_binary(Bin) ->
    <<Acc/binary, Bin/binary, " ">>;
flatten(Atom, Acc) when is_atom(Atom) ->
    <<Acc/binary, (a2b(Atom))/binary, " ">>;
flatten(T, Acc) when is_tuple(T) ->
    Bin = flatten(tuple_to_list(T)),
    <<Acc/binary, "{", Bin/binary, "} ">>;
flatten(L, Acc) when is_list(L) ->
    case is_string(L) of
        true ->
            <<Acc/binary, (list_to_binary(L))/binary, " ">>;
        _ ->
            <<Acc/binary, (flatten(L))/binary, " ">>
    end;
flatten(I, Acc) when is_integer(I) ->
    <<Acc/binary, (integer_to_binary(I))/binary, " ">>;
flatten(Pid, Acc) when is_pid(Pid) ->
    Name = case erlmud_index:get(Pid) of
               Atom when is_atom(Atom) ->
                   a2b(Atom);
               List when is_list(List) ->
                   l2b(List);
               Other ->
                   io:format(user, "Pid name is not atom or list: ~p~n", [Other]),
                   no_name
           end,
    PidBin = p2b(Pid),
    <<Acc/binary, "{", Name/binary, ": ", PidBin/binary, "} " >>;
flatten(X, Acc) ->
    io:format(user, "Not logging value ~p in log string ~p~n", [X, Acc]),
    Acc.

is_string([]) ->
    true;
is_string([X | Rest]) when is_integer(X),
                           X > 9, X < 127 ->
    is_string(Rest);
is_string(_) ->
    false.

l2b(List) when is_list(List) ->
    list_to_binary(List).

p2b(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

a2b(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

i2b(Int) when is_integer(Int) ->
    integer_to_binary(Int).

ts2b({Meg, Sec, Mic}) ->
    MegBin = i2b(Meg),
    SecBin = i2b(Sec),
    MicBin = i2b(Mic),
    <<"{", MegBin/binary, ",", SecBin/binary, ",", MicBin/binary, "}">>.
