-module(gloom_protocol).

-export([start_link/3, init/3]).

-type state() :: any().
-type message() :: string().
-type reply() ::
    {reply, message(), state()}
    | pop
    | {push, module(), state()}
    | {noreply, state()}
    | {update, module(), state()}.

-callback init(any()) -> {ok, state()}.
-callback recv(message(), state()) -> reply().
-callback tick(state()) -> reply().

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, [Handler | Opts]) ->
    {ok, Socket} = ranch:handshake(Ref),
    {ok, State} = Handler:init(Opts),
    loop(Socket, Transport, [{Handler, State}]).

loop(Socket, Transport, [{Handler, State} | StateQueue]) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            case Handler:recv(string:trim(Data), State) of
                stop ->
                    Transport:close(Socket);
                {push, NewHandler, NewState} ->
                    loop(Socket, Transport, [{NewHandler, NewState}, {Handler, State} | StateQueue]);
                pop ->
                    [NewState | NewQueue] = StateQueue,
                    loop(Socket, Transport, [NewState | NewQueue]);
                {reply, Reply, NewState} ->
                    Transport:send(Socket, Reply),
                    loop(Socket, Transport, [{Handler, NewState} | StateQueue]);
                {noreply, NewState} ->
                    loop(Socket, Transport, [{Handler, NewState} | StateQueue])
            end;
        {error, timeout} ->
            lager:info("~p timed out.", [Socket]);
        {error, closed} ->
            lager:info("~p closed.", [Socket])
    end.
