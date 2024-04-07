-module(gloom).

-export([start_listener/3, start_listener/4]).

-export([send/2, send/3, prompt/3]).

-type prompt_type() :: string() | {string(), [string()]}.

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

-spec send(pid(), string()) -> ok.
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

-spec send(pid(), string(), [term]) -> ok.
send(Pid, Data, Args) ->
    gen_server:cast(Pid, {send, io_lib:format(Data, Args)}).

-spec prompt(pid(), atom(), [{atom(), prompt_type()}]) -> ok.
prompt(Pid, Name, Prompts) ->
    gen_server:cast(Pid, {prompt, Name, Prompts}).
