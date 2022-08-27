%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_event_log).

-behaviour(gen_server).

-include("include/gerlshmud.hrl").

-export([start_link/0]).
-export([log/2]).
-export([log/3]).
-export([register/1]).
-export([flatten/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {log_file :: file:io_device(),
                loggers = [] :: [function()]}).

%% API

log(Level, Terms) when is_atom(Level) ->
    Self = self(),
    log(Self, Level, Terms).

log(Pid, Level, Terms) when is_atom(Level) ->
    case whereis(?MODULE) of
        undefined ->
            %io:format(user,
                      %"gerlshmud_event_log process not found~n"
                      %"Pid: ~p, Level: ~p, Terms: ~p~n",
                      %[Pid, Level, Terms]);
            ok;
        _ ->
            gen_server:cast(?MODULE, {log, Pid, Level, Terms})
    end.

register(Logger) when is_function(Logger) ->
    gen_server:cast(?MODULE, {register, Logger}).

flatten(Props) ->
    Flattened = flatten(Props, []),
    lists:ukeysort(1, Flattened).

flatten([], List) ->
    List;
flatten([{K, [{K2, V} | L]} | Rest], Out) ->
    flatten([{K, L} | Rest], [{K2, V} | Out]);
flatten([T | Rest], Out) when is_tuple(T) ->
    flatten(Rest, [T | Out]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server

init([]) ->
    process_flag(priority, max),
    io:format("Starting logger (~p)~n", [self()]),
    LogPath = get_log_path(),
    {ok, LogFile} = file:open(LogPath ++ "/gerlshmud.log", [append]),

    io:format(user, "Logger:~n\tLog file: ~p~n", [LogFile]),

    {ok, #state{log_file = LogFile}}.

handle_call(Request, From, State) ->
    io:format(user, "gerlshmud_event_log:handle_call(~p, ~p, ~p)~n",
              [Request, From, State]),
    {reply, ignored, State}.

handle_cast({log, Pid, Level, Props}, State) when is_list(Props) ->
    Props2 = [{process, Pid}, {level, Level} | Props],
    NamedProps = add_index_details(Props2),
    BinProps = [{flatten_key(json_friendly(K)), json_friendly(V)} || {K, V} <- NamedProps],
    JSON2 =
    try
        JSON = jsx:encode(BinProps),
        ok = file:write(State#state.log_file, <<JSON/binary, "\n">>),
        JSON
    catch
        Error ->
            io:format(user, "~p caught error:~n\t~p~n", [?MODULE, Error])
    end,
    [call_logger(Logger, Level, JSON2) || Logger <- State#state.loggers],
    {noreply, State};

handle_cast({register, Logger}, State = #state{loggers = Loggers}) ->
    {noreply, State#state{loggers = [Logger | Loggers]}};

handle_cast(Msg, State) ->
    io:format(user, "Unrecognized cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format(user, "~p:handle_info(~p, State)~n", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format(user, "Terminating gerlshmud_event_log: ~p~n~p~n", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% util

get_log_path() ->
    case os:getenv("GERLSHMUD_LOG_PATH") of
        false ->
            {ok, CWD} = file:get_cwd(),
            CWD;
        Path ->
            Path
    end.

call_logger(Logger, Level, JSON) ->
    try
        Logger(Level, JSON)
    catch
        Error ->
            io:format(user,
                      "~p caught error calling Logger function:~n\t~p~n",
                      [?MODULE, Error])
    end.

flatten_key([A1, A2]) when is_atom(A1), is_atom(A2) ->
    B1 = atom_to_binary(A1, utf8),
    B2 = atom_to_binary(A2, utf8),
    <<B1/binary, "_", B2/binary>>;
flatten_key(Other) ->
    Other.

json_friendly(List) when is_list(List) ->
    case is_string(List) of
        true ->
            l2b(List);
        false ->
            [json_friendly(E) || E <- List]
    end;
json_friendly(Timestamp = {Meg, Sec, Mic})
  when is_integer(Meg), is_integer(Sec), is_integer(Mic)  ->
    ts2b(Timestamp);
json_friendly(Tuple) when is_tuple(Tuple) ->
    json_friendly(tuple_to_list(Tuple));
json_friendly(Ref) when is_reference(Ref) ->
    ref2b(Ref);
json_friendly(Pid) when is_pid(Pid) ->
    p2b(Pid);
json_friendly(Any) ->
    Any.

add_index_details(Props) ->
    lists:foldl(fun add_index_details/2, [], Props).

add_index_details({_Key = {Atom1, Atom2}, Pid}, NamedProps)
  when is_pid(Pid),
       is_atom(Atom1),
       is_atom(Atom2) ->
    Str1 = atom_to_list(Atom1),
    Str2 = atom_to_list(Atom2),
    Key = list_to_atom(Str1 ++ "_" ++ Str2),
    add_index_details({Key, Pid}, NamedProps);
add_index_details({Key, Pid}, NamedProps) when is_pid(Pid) ->
    case gerlshmud_index:get(Pid) of
        undefined ->
            NamedProps;
        #object{id = Id, icon = Icon} ->
            IdKey = list_to_atom(atom_to_list(Key) ++ "_id"),
            IconKey = list_to_atom(atom_to_list(Key) ++ "_icon"),
            Props = [P || P = {_, V} <- [{IdKey, Id}, {IconKey, Icon}], V /= undefined],
            [{Key, Pid} | Props] ++ NamedProps
    end;
add_index_details(Prop, NamedProps) ->
    [Prop | NamedProps].

is_string([]) ->
    true;
is_string([X | Rest]) when is_integer(X),
                           X > 9, X < 127 ->
    is_string(Rest);
is_string(_) ->
    false.

l2b(List) when is_list(List) ->
    list_to_binary(List).

ref2b(Ref) when is_reference(Ref) ->
    list_to_binary(ref_to_list(Ref)).

p2b(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%a2b(Atom) when is_atom(Atom) ->
    %list_to_binary(atom_to_list(Atom)).

i2b(Int) when is_integer(Int) ->
    integer_to_binary(Int).

ts2b({Meg, Sec, Mic}) ->
    MegBin = i2b(Meg),
    SecBin = i2b(Sec),
    MicBin = i2b(Mic),
    <<"{", MegBin/binary, ",", SecBin/binary, ",", MicBin/binary, "}">>.
