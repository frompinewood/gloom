-module(gloom).

-export([start_listener/1]).

start_listener(Name) ->
    {ok, Port} = application:get_env(port),
    {ok, Handler} = application:get_env(gloom_handler),
    ranch:start_listener(
        Name,
        ranch_tcp,
        [{port, Port}],
        gloom_protocol,
        [Handler]
    ).
