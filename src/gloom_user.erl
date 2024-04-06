-module(gloom_user).
-export([init/1, handle_call/3, start_link/0]).
-export([create/2, verify/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ets:new(?MODULE, [set])}.

create(User, Pass) -> gen_server:call(?MODULE, {create, User, Pass}).
verify(User, Pass) -> gen_server:call(?MODULE, {verify, User, Pass}).

handle_call({create, User, _}, _From, Table) when
    length(User) < 3
->
    {reply, {error, bad_username}, Table};
handle_call({create, User, Pass}, _From, Table) ->
    Reply =
        case ets:lookup(Table, User) of
            [_] ->
                {error, user_exists};
            [] ->
                {ok, Salt} = bcrypt:gen_salt(),
                {ok, Hash} = bcrypt:hashpw(Pass, Salt),
                Id = make_ref(),
                ets:insert_new(Table, {User, Hash, Id}),
                {ok, Id}
        end,
    {reply, Reply, Table};
handle_call({verify, User, Pass}, _From, Table) ->
    Reply =
        case ets:lookup(Table, User) of
            [] ->
                {error, bad_username};
            [{_, Hash, Id}] ->
                {ok, NewHash} = bcrypt:hashpw(Pass, Hash),
                case NewHash =:= Hash of
                    true -> {ok, Id};
                    false -> {error, bad_password}
                end
        end,
    {reply, Reply, Table}.
