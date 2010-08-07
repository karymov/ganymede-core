-module(sql).

-export([select/2, insert/1, insert2/2, delete/2, update/2]).
-export([equery/2]).

-include("pgsql.hrl").
-include("ganymede_sql.hrl").

connect() ->
    C = sql_con:get(),
    put(c, C),
    C.

close() ->
    sql_con:get_back(get(c)).

select(RecordTag, ID) ->
    TableName = sql_table(RecordTag),
    connect(),
    {ok, _, R} = pgsql:equery(get(c), "SELECT * FROM "++TableName++" WHERE id=$1",[ID]),
    close(),
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
    connect(),
    {ok, _, _, [{ID}]} = pgsql:equery(get(c), "INSERT INTO "++TableName++P1++" VALUES "++P2++" returning id", Parameters),
    close(),
    {ok, ID}.

insert2(Record, ID) ->
    [RecordTag | [_|Parameters]] = tuple_to_list(Record),
    {P1, P2} = sql_insert2_pattern(RecordTag),
    TableName = sql_table(RecordTag),
    connect(),
    {ok, 1} = pgsql:equery(get(c), "INSERT INTO "++TableName++P1++" VALUES "++P2, [ID|Parameters]),
    close(),
    {ok, ID}.

delete(RecordTag, ID) ->
    TableName = sql_table(RecordTag),
    connect(),
    Result = pgsql:equery(get(c), "DELETE FROM "++TableName++" WHERE id=$1",[ID]),
    close(),
    Result.

update(ID, Record) ->
    [RecordTag | [_ | Parameters]] = tuple_to_list(Record),
    Pattern = sql_update_pattern(RecordTag),
    TableName = sql_table(RecordTag),
    connect(),
    Result = pgsql:equery(get(c), "UPDATE "++TableName++" SET "++Pattern++" WHERE id=$1", [ID |Parameters]),
    close(),
    Result.

equery(SQL, Parameters) ->
    connect(),
    Result = pgsql:equery(get(c), SQL, Parameters),
    close(),
    Result.

make_record(RecordTag, RList) ->
    list_to_tuple([RecordTag | tuple_to_list(RList)]).