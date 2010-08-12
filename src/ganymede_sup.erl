-module(ganymede_sup).

-export([start_link/0]).
-export([init/1]).

-behavior(supervisor).


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_Args) ->
    
    Accounts = {accounts,
                {accounts, start_link, []},
                permanent, 5000, worker, dynamic},
    MetaTree = {meta_tree,
                {meta_tree, start_link, []},
                permanent, 5000, worker, dynamic},
    MetaDB = {meta_db,
                {meta_db, start_link, []},
                permanent, 5000, worker, dynamic},
    SqlCon = {sql_con,
                {sql_con, start_link, []},
                permanent, 5000, worker, dynamic},
     
    {ok, {{one_for_one, 1, 60}, [Accounts, MetaTree, SqlCon]}}.