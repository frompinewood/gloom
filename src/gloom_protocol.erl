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

handle_cast({send, Data}, {T, S, _, _} = State) ->
    T:send(S, [Data, $\n]),
    {noreply, State};
handle_cast({prompt, Name, Prompts}, {T, S, H, P} = State) ->
    T:setopts(S, [{active, false}]),
    Response = lists:foldl(
        fun({Tag, Prompt}, Acc) ->
            T:send(S, Prompt),
            {ok, Data} = T:recv(S, 0, 60000),
            Acc#{Tag => string:trim(Data)}
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
