-module(gloom).

-export([start_listener/3, start_listener/4]).

-spec start_listener(term(), non_neg_integer(), module()) -> {ok, pid()}.
start_listener(Name, Port, CallbackModule) ->
    start_listener(Name, Port, CallbackModule, []).

-spec start_listener(term(), non_neg_integer(), module(), list()) -> {ok, pid()}.
start_listener(Name, Port, CallbackModule, Args) ->
    ranch:start_listener(
        Name,
        ranch_tcp,
        [{port, Port}],
        gloom_protocol,
        [CallbackModule | Args]
    ).
