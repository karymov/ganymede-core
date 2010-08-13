-module(sql).

-export([select/2, insert/1, insert2/2, delete/2, update/2]).
-export([equery/2]).

-include("pgsql.hrl").

sql_table(account) -> " accounts ";
sql_table(node) -> " nodes ";
sql_table(rsc_meta) -> " rsc_metas ";
sql_table(person) -> " people ";
sql_table(publisher) -> " publishers ";
sql_table(fileinfo) -> " fileinfos ";
sql_table(cover) -> " covers ";
sql_table(tag) -> " tags ".

sql_insert_pattern(account) ->
    {"(login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"};
sql_insert_pattern(node) ->
    {"(name,type,parent)",
    "($1,$2,$3)"};
sql_insert_pattern(rsc_meta) ->
    {"(name,type,author,description,year,discipline,url,cover,pages_count,size,publisher,duration,file,tag)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14)"}.

sql_insert2_pattern(account) ->
    {"(id,login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)"};
sql_insert2_pattern(node) ->
    {"(id,name,type,parent)",
    "($1,$2,$3,$4)"};
sql_insert2_pattern(rsc_meta) ->
    {"(id,name,type,author,description,year,discipline,url,cover,pages_count,size,publisher,duration,file,tag)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)"}.


sql_update_pattern(account) ->
    "person_id=$2,password=$3,name=$4,surname=$5,role=$6,state=$7,reg_datetime=$8,login_datetime=$9,description=$10,email=$11";
sql_update_pattern(node) ->
    "name=$2,type=$3,children=$4";
sql_update_pattern(rsc_meta) ->
    "name=$2,type=$3,author=$4,description=$5,year=$6,discipline=$7,url=$8,cover=$9,pages_count=$10,size=$11,publisher=$12,duration=$13,file=$14,tag=$15".

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