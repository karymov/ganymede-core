%%--------------------------------------------------------------------
%% SqlDataBase Driver Adapter
%% this module implements abstraction layer for specific RDBMS
%% PostgreSQL
%%--------------------------------------------------------------------

-module(sql).

-export([connect/0, connect/5, close/0]).
-export([select/2, insert/1, delete/2, update/2]).
-export([equery/2]).
% -export([test1/0, test2/1]).


-include("pgsql.hrl").
-include("ganymede_sql.hrl").
% -include("ganymede.hrl").

connect() ->
    Host = "localhost",
    Port = 5432,
    Database = "ganymede",
    Username = "postgres",
    Password = "postgres",
    connect(Host, Port, Database, Username, Password).
connect(Host, Port, Database, Username, Password) ->
    {ok, Pid} = pgsql:connect(Host, Username, Password, [{port, Port},{database, Database}]),
    put(c, Pid).

close() ->
    pgsql:close(get(c)).

select(RecordTag, ID) ->
    TableName = sql_table(RecordTag),
    ?MODULE:connect(),
    {ok, _, R} = pgsql:equery(get(c), "SELECT * FROM "++TableName++" WHERE id=$1",[ID]),
    ?MODULE:close(),
    case lists:map(fun(RList) -> make_record(RecordTag, RList) end, R) of
        [] ->
            undefined;
        [Row] ->
            Row
    end.

insert(Record) ->
    [RecordTag | [_ | Parameters]] = tuple_to_list(Record),
    {P1, P2} = sql_insert_pattern(RecordTag),
    TableName = sql_table(RecordTag),
    ?MODULE:connect(),
    {ok, _, _, [{ID}]} = pgsql:equery(get(c), "INSERT INTO "++TableName++P1++" VALUES "++P2++" returning id", Parameters),
    ?MODULE:close(),
    {ok, ID}.

delete(RecordTag, ID) ->
    TableName = sql_table(RecordTag),
    ?MODULE:connect(),
    Result = pgsql:equery(get(c), "DELETE FROM "++TableName++" WHERE id=$1",[ID]),
    ?MODULE:close(),
    Result.

update(ID, Record) ->
    [RecordTag | [_ | Parameters]] = tuple_to_list(Record),
    Pattern = sql_update_pattern(RecordTag),
    TableName = sql_table(RecordTag),
    ?MODULE:connect(),
    Result = pgsql:equery(get(c), "UPDATE "++TableName++" SET "++Pattern++" WHERE id=$1", [ID |Parameters]),
    ?MODULE:close(),
    Result.

equery(SQL, Parameters) ->
    ?MODULE:connect(),
    Result = pgsql:equery(get(c), SQL, Parameters),
    ?MODULE:close(),
    Result.

make_record(RecordTag, RList) ->
    list_to_tuple([RecordTag | tuple_to_list(RList)]).
    
% test1() ->
%     A = #account{name = "John", reg_datetime = {date(),time()}, login_datetime = {date(),time()}},
%     ?MODULE:insert(A).
% 
% test2(_) ->
%     A = #account{name = "John", reg_datetime = {date(),time()}, login_datetime = {date(),time()}},
%     %B = #account{name = "Jack"},
%     B = A#account{name = "Jack"},
%     {ok, ID} = ?MODULE:insert(A),
%     update(ID,B).
