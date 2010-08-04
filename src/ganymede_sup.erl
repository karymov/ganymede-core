-module(ganymede_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_Args) ->
    
    % Accounts manager
    Accounts = {accounts,
                {accounts, start_link, []},
                permanent, 5000, worker, dynamic},
    % Metastore
    Metastore = {metastore,
                {metastore, start_link, []},
                permanent, 5000, worker, dynamic},
    
    % Database connection
    %Database = 
    %% g_db:start_link(default),
     
    {ok, {{one_for_one, 1, 60}, [Accounts, Metastore]}}.