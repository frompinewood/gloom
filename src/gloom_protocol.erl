-module(gloom_protocol).

-export([start_link/3, init/3]).
-export_type([state/0, message/0, reply/0]).

-type state() :: any().
-type message() :: string().
-type reply() ::
    {reply, message(), state()}
    | {pop, state()}
    | {push, module(), any()}
    | {noreply, state()}
    | {update, module(), state()}.

-callback init(any()) -> {ok, state()}.
-callback recv(message(), state()) -> reply().
-callback tick(state()) -> reply().

-optional_callbacks([tick/1]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, [Handler | Opts]) ->
    {ok, Socket} = ranch:handshake(Ref),
    case Handler:init(Opts) of 
        {ok, State} -> 
            loop(Socket, Transport, [{Handler, State}]);
        {reply, Message, State} ->
            Transport:send(Socket, Message),
            loop(Socket, Transport, [{Handler, State}])
    end.

loop(Socket, Transport, [{Handler, State} | StateQueue]) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            case Handler:recv(string:trim(Data), State) of
                stop ->
                    Transport:close(Socket);
                {push, NewHandler, Opts} ->
                    NextState = case NewHandler:init(Opts) of
                        {ok, NewState} -> NewState;
                        {reply, Message, NewState} ->
                            Transport:send(Socket, Message),
                            NewState
                    end,
                    loop(Socket, Transport, [{NewHandler, NextState}, {Handler, State} | StateQueue]);
                {pop, NewState} ->
                    [{OldState, OldHandler} | OldQueue] = StateQueue,
                    FixedState = OldHandler:resolve_state(NewState, OldState),
                    loop(Socket, Transport, [{FixedState, OldHandler} | OldQueue]);
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
