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
    T:send(S, Data),
    {noreply, State}.

handle_info({tcp_closed, S}, {_,S,_,_} = State) -> 
    lager:info("~p closed.", [S]),
    {stop, normal, State};
handle_info({tcp, S, Data}, {_,S,H,P} = State) ->
    H:send(P, string:trim(Data)),
    {noreply, State}.

terminate(normal, {_,_,_,P}) ->
    lager:info("Stopped ~p.", [P]),
    ok.
