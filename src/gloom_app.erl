%%%-------------------------------------------------------------------
%% @doc gloom public API
%% @end
%%%-------------------------------------------------------------------

-module(gloom_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gloom_sup:start_link().

stop(_State) ->
    ranch:stop_listener(gloom),
    ok.

%% internal functions
