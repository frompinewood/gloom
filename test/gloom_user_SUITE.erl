-module(gloom_user_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create_user/1, verify_user/1]).

init_per_suite(Config) ->
    application:ensure_all_started([gloom]),
    Config.

end_per_suite(_Config) -> ok.

all() ->
    [create_user, verify_user].

create_user(_) ->
    {ok, Id} = gloom_user:create("bob", "bobpass"),
    true = is_reference(Id).

verify_user(_) ->
    {ok, Id} = gloom_user:create("alice", "alicepass"),
    true = {ok, Id} =:= gloom_user:verify("alice", "alicepass"),
    true = {error, bad_password} =:= gloom_user:verify("alice", "bobpass").
