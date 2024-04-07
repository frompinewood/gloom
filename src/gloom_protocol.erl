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
    T:setopts(S, [{active, false}]),
    Response = lists:foldl(
        fun(Prompt, Acc) ->
            maps:merge(Acc, prompt(Prompt, T, S))
        end,
        #{},
        Prompts
    ),
    H:send(P, {Name, Response}),
    T:setopts(S, [{active, true}]),
    {noreply, State}.

handle_info({tcp_closed, S}, {_, S, _, _} = State) ->
    lager:info("~p closed.", [S]),
    {stop, normal, State};
handle_info({tcp, S, Data}, {_, S, H, P} = State) ->
    H:send(P, string:trim(Data)),
    {noreply, State}.

terminate(normal, {_, _, _, P}) ->
    lager:info("Stopped ~p.", [P]),
    ok.

prompt({Tag, Prompt}, Transport, Socket) ->
    Transport:send(Socket, Prompt),
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} -> #{Tag => string:trim(Data)};
        {error, _} -> prompt({Tag, Prompt}, Transport, Socket)
    end;
prompt({Tag, Prompt, Options}, Transport, Socket) ->
    Transport:send(Socket, [string:join(Options, "\n"),"\n", Prompt]),
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} ->
            Data1 = string:trim(Data),
            case
                lists:member(
                    binary_to_list(string:lowercase(Data1)),
                    lists:map(fun string:lowercase/1, Options)
                )
            of
                true -> #{Tag => Data1};
                false -> prompt({Tag, Prompt, Options}, Transport, Socket)
            end;
        {error, _} ->
            prompt({Tag, Prompt, Options}, Transport, Socket)
    end.
