-module(sql_con).
-behavior(gen_server).

-export([start_link/0, start_link/1, stop/0, get/0, get_back/1, monitor/0]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() -> start_link(["localhost", 5432, "ganymede", "postgres", "postgres"]).
start_link([Host, Port, Database, Username, Password]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Database, Username, Password], []).
    
get() -> gen_server:call(?MODULE, get).

get_back(Connection) -> gen_server:call(?MODULE, {get_back, Connection}).

stop() -> gen_server:cast(?MODULE, stop).

monitor() -> gen_server:call(?MODULE, monitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ConnectionOpts) -> {ok, ConnectionOpts}.

handle_call(get, _From, ConnectionOpts) ->
    {reply, get_connection(ConnectionOpts), ConnectionOpts};
            
handle_call({get_back, Connection}, _From, ConnectionOpts) ->
    put_connection(Connection),
    {reply, {ok, put}, ConnectionOpts};
    
handle_call(monitor, _From, ConnectionOpts) ->
    monitor_connections(ConnectionOpts),
    {reply, ok, ConnectionOpts}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    close_connections().
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connections() -> lists:filter(
    fun(Term) ->
        case Term of 
            {{c, _}, _} -> true;
            _ -> false
        end
    end, erlang:get()).

create_connection([Host, Port, Database, Username, Password]) ->
    {ok, C} = pgsql:connect(Host, Username, Password, [{port, Port},{database, Database}]),
    C.

since() ->
    {X, Y, Z} = now(),
    (X * 1000000000 + Y * 1000000 + Z) div 1000.
    
put_connection(C) -> put({c, C}, since()).

get_connection(ConnectionOpts) ->
    case connections() of
        [] -> create_connection(ConnectionOpts);
        [{{c, C}, _}| _] -> C
    end.
    
close_connection(C) -> pgsql:close(C).
close_connections() -> lists:foreach(fun({{c, C}, _}) -> close_connection(C) end, connections()).

monitor_connections(ConnectionOpts) ->
    io:format("MONITOR:~n", []),
    io:format("   options: ~p;~n", [ConnectionOpts]),
    lists:foreach(
        fun({{c, C}, S}) ->
            io:format("   *pid: ~p; since: ~p;~n", [C, S])
        end,
    connections()).