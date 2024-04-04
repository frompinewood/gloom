-module(gloom).

-export([start_listener/3]).

-spec start_listener(atom(), list(), module()) -> 
    {ok, pid()} |
    {error, any()}.
start_listener(Name, TranOpts, Handler) ->
    ranch:start_listener(
        Name,
        ranch_tcp,
        TranOpts,
        gloom_protocol,
        [Handler]
    ).
