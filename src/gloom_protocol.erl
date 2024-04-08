-module(gloom_protocol).

-export([start_link/3, init/3, terminate/2, handle_info/2, handle_cast/2]).

start_link(Ref, Transport, Args) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Transport, Args]),
    {ok, Pid}.

init(Ref, Transport, [Handler | Args]) ->
    {ok, Socket} = ranch:handshake(Ref),
    Transport:setopts(Socket, [{active, true}]),
    lager:info("~p connected.", [Socket]),
    {ok, Pid} = Handler:start_link(self(), Args),
    lager:info("Started ~p.", [Pid]),
    gen_server:enter_loop(?MODULE, [], {Transport, Socket, Handler, Pid}).

handle_cast({send, close}, {T, S, _, _} = State) ->
    T:close(S),
    {stop, normal, State};
handle_cast({send, Data}, {T, S, _, _} = State) ->
    T:send(S, [Data, $\n]),
    {noreply, State};
handle_cast({prompt, Name, Prompts}, {T, S, H, P} = State) ->
    Fun =
        fun(Prompt, Acc) ->
            maps:merge(Acc, prompt(Prompt, T, S))
        end,
    T:setopts(S, [{active, false}]),
    Response = lists:foldl(Fun, #{}, Prompts),
    H:send(P, {Name, Response}),
    T:setopts(S, [{active, true}]),
    {noreply, State}.

handle_info({tcp_closed, S}, {_, S, _, _} = State) ->
    {stop, normal, State};
handle_info({tcp, S, Data}, {_, S, H, P} = State) ->
    H:send(P, string:trim(Data)),
    {noreply, State}.

terminate(normal, _) -> ok.

prompt({Tag, Prompt}, Transport, Socket) ->
    Transport:send(Socket, Prompt),
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} -> #{Tag => string:trim(Data)};
        {error, _} -> prompt({Tag, Prompt}, Transport, Socket)
    end;
prompt({Tag, Prompt, Options}, Transport, Socket) ->
    OptionText = format_options(Options),
    Transport:send(Socket, [$\n, OptionText, Prompt]),
    Choice =
        case Transport:recv(Socket, 0, 60000) of
            {ok, Data} -> validate_choice(Options, string:trim(Data));
            {error, timeout} -> exit(normal);
            {error, _} -> prompt({Tag, Prompt, Options}, Transport, Socket)
        end,
    ValidChoice =
        case Choice of
            {error, _} ->
                prompt({Tag, Prompt, Options}, Transport, Socket);
            Value ->
                Value
        end,
    #{Tag => ValidChoice}.

format_options(Options) ->
    lists:map(
        fun({I, O}) ->
            io_lib:format("~p ~s~n", [I, O])
        end,
        lists:enumerate(Options)
    ).

validate_choice(Options, Data) ->
    case
        lists:search(
            fun(Option) ->
                string:lowercase(Option) =:= string:lowercase(Data)
            end,
            lists:map(fun list_to_binary/1, Options)
        )
    of
        {value, Value} ->
            Value;
        false ->
            try
                lists:nth(binary_to_integer(Data), Options)
            catch
                error:Reason:_ -> {error, Reason}
            end
    end.
