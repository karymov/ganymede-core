%%--------------------------------------------------------------------
%% SqlDataBase Driver Adapter
%% this module implements abstraction layer for specific RDBMS
%% PostgreSQL
%%--------------------------------------------------------------------

-module(g_db).

-export([start_link/1, start_link/5, select/1]).
-export([insert/1, insert/3, delete/1, update/1]).
-export([update/4, create_table/2, not_null/1, if_null_to_undf/1, hash_str/1]).
-export([pack/4, serialize_str_list/1, parse_str_list/1, parse_atom/1, to_str/1]).
-export([test1/0, test2/0, test3/0]).
%-export([squery/1, pack_to_brackets/1]).

-include("pgsql.hrl").
-include("ganymede.hrl").

-define(host, "localhost").
-define(port, 5432).


%%--------------------------------------------------------------------
%% 
%% Module interface
%%
%%--------------------------------------------------------------------

%% @doc
%% @spec
start_link(default) ->
    put(host, ?host),
    put(port, ?port),
    put(database, "ganymede"),
    put(username, "postgres"),
    put(password, "postgres"),
    ok.
    
%% @doc
%% @spec
start_link(Host, Port, Database, User, Password) ->
    put(host, Host),
    put(port, Port),
    put(database, Database),
    put(username, User),
    put(password, Password),
    ok.
    
%% @doc
%% @spec  
select(Query) ->
    case squery(Query) of
        {ok, _Cols, Rows} ->
            Rows;
        {error, Reason} ->
            io:format("Error: ~p~n",[Reason]);
        Other ->
            io:format("Error: ~p~n",[Other]),
            {error, Other}
    end.

%% @doc
%% @spec
insert(Query) ->
    squery(Query).

%% @doc
%% @spec
insert(Record, #g_record_info{} = RecordInfo, Into) ->
    PropList = record_to_proplist(Record, RecordInfo),
    %%io:format("INSERT Record: ~p~n",[PropList]),
    {F, V} = proplist_to_sql(PropList),
    Query = "INSERT INTO " ++ Into ++ " " ++ pack_to_brackets(F) ++ " VALUES " ++ pack_to_brackets(V),
    squery(Query).

%% @doc
%% @spec
delete(Query) ->
    squery(Query).

%% @doc
%% @spec
update(Query) ->
    squery(Query).
    
%% @doc
%% @spec
update(Record, #g_record_info{} = RecordInfo, Table, Where) -> 
    PropList = record_to_proplist(Record, RecordInfo),
    {F, V} = proplist_to_sql(PropList),    
    Query = "UPDATE " ++ Table ++ " SET " ++ pack_to_set_str(F, V) ++ " " ++ Where,
    squery(Query). 

%% @doc
%% @spec
create_table(_RecordInfo, _Name) ->
    ok.

%%--------------------------------------------------------------------
%% 
%% Utilities
%%
%%--------------------------------------------------------------------

%% @doc
%% @spec
squery2(Query) when erlang:is_list(Query) ->
    case connect() of
        {ok, Pid} ->            
            Result = pgsql:squery(Pid, Query),
            pgsql:close(Pid),
            flush([]),
            Result;
        Error -> io:format("DBA Error:~p~n", [Error])
    end.

squery(Query) when erlang:is_list(Query) ->
    P = case get(p1) of
        undefined ->
            {ok, Pid} = connect(),
            put(p1, Pid),
            Pid;
        Pid -> Pid
    end,
    pgsql:squery(P, Query).

%% @doc
%% @spec
flush(Acc) ->
    receive
        {'EXIT', _Pid, normal} ->
            flush(Acc);
        M  ->
            flush([M | Acc])
    after
        0 ->
            lists:reverse(Acc)
    end.
 
%% @doc
%% @spec 
connect() ->
    Host = get(host), 
    Port = get(port), 
    Database = get(database),
    Username = get(username),
    Password = get(password),
    pgsql:connect(Host, Username, Password, [{port, Port},{database, Database}]).

%% @doc
%% @spec record_to_proplist(#record{}, #g_record_info{}) ->
%% @spec [{fieldAtom1, {typeAtom1, Value1}}| ...]  
record_to_proplist(Record, #g_record_info{} = Info) when erlang:is_tuple(Record) ->
    [_Tag| Values] = tuple_to_list(Record),
    lists:zipwith(
        fun(Value, {Field, Type}) ->
            {Field, {Type, Value}}
        end,
        Values, Info#g_record_info.info
    ).

%% @doc
%% @spec proplist_to_sql(PropList) -> {"(field1, field2, ..)", "(value1, value2, ...)"}
proplist_to_sql(PropList) ->
    DefinedValues = [{Field, {Type, Value}} 
        || {Field, {Type, Value}} <- PropList, not_null(Value)],
    FieldsList = [io_lib:write_atom(Field) || {Field, _TypeValue} <- DefinedValues],
    ValuesList = lists:map(
        fun({_Field, {Type, Value}}) ->
           erlang_type_to_sql_str(Type, Value)
        end,
        DefinedValues
    ),
    {FieldsList, ValuesList}.

%% @doc
%% @spec
pack_to_brackets(StringList) ->
    pack(StringList, ",", "(", ")").

%% @doc
%% @spec
pack([Value], Separator, LBracket, RBracket) ->
    LBracket ++ Value ++ RBracket;
pack([], _Separator, LBracket, RBracket) ->
    LBracket ++ RBracket;
pack([Head| Tail], Separator, LBracket, RBracket) ->
    LBracket ++ Head ++ lists:foldl(fun(X,Acc)->Acc++Separator++X end, [], Tail)++RBracket.

%% @doc
%% @spec set string is a string like 'key1=value1,key2=value2...'
pack_to_set_str([Field],[Value]) ->
    Field ++ "=" ++ Value;
pack_to_set_str([FieldsHead| FieldsTail] = Fields, [ValuesHead| ValuesTail] = Values)
    when (length(Fields) =:=  length(Values)) andalso (length(Fields) > 1) ->
        FieldValueList = lists:zipwith(fun(F,V)->{F,V} end, FieldsTail, ValuesTail),
        FieldsHead ++ "=" ++ ValuesHead ++ lists:foldl(fun({F,V},A)->A++","++F++"="++V end, [], FieldValueList).

%% @doc
%% @spec
erlang_type_to_sql_str(Type, Value) ->
    Base = ?MODULE:to_str(Value),
    case Type of
        int -> Base;
        str_list -> "'" ++ serialize_str_list(Base) ++ "'";
        _Unknown -> "'"++Base++"'"
    end.

%% @doc
%% @spec
to_str(Value) ->
    Base = if
        is_integer(Value) -> integer_to_list(Value);
        is_binary(Value) -> binary_to_list(Value);
        is_list(Value) -> Value;
        is_atom(Value) -> atom_to_list(Value)
    end.


%% @doc
%% @spec
not_null(undefined) -> false;
not_null(null) -> false;
not_null([]) -> false;
not_null(<<>>) -> false;
not_null(Value) -> true.

%% @doc
%% @spec
if_null_to_undf(Value) ->
    case ?MODULE:not_null(Value) of
        true -> Value;
        false -> undefined
    end.

%% @doc
%% @spec
hash_str(String) ->
    Bytes = crypto:sha(String),
    List = binary_to_list(Bytes),
    lists:flatten(list_to_hex(List)).

%% @doc
%% @spec    
list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

%% @doc
%% @spec
serialize_str_list(List) ->
    case ?MODULE:not_null(List) of
        true ->
            ListOfStrings = lists:map(fun ?MODULE:to_str/1, List),
            pack(ListOfStrings, ",", [], []);
        false ->
            []
    end.

%% @doc
%% @spec        
parse_str_list(StrList) ->
    case not_null(StrList) of
        true -> re:split(?MODULE:to_str(StrList),",", [{return, binary}]);
        false -> []
    end.


%% @doc
%% @spec        
parse_atom(Atom) ->
    case not_null(Atom) of
        true -> list_to_atom(?MODULE:to_str(Atom));
        false -> undefined
    end.

%%--------------------------------------------------------------------
%% 
%% Tests
%%
%%--------------------------------------------------------------------

test1() ->
    Record = #account{ id = "test_id", name = "test_name", role = 1},
    Info = g_record_info(account),
    PropList = record_to_proplist(Record, Info),
    Sql = proplist_to_sql(PropList),
    
    io:format("test1 Record:~p~n", [Record]),
    io:format("test1 Info:~p~n", [Info]),
    io:format("test1 PropList:~p~n", [PropList]),
    io:format("test1 Sql:~p~n", [Sql]),
    
    insert(Record, Info, "accounts").

test2() ->
    % Record1 = #account{ id = "test_id_1", name = "test_name_1", role = 1},
    Record2 = #account{ id = "test_id_1", name = "test_name_1_u2", surname = "test_surname", role = 1},   
    Info = g_record_info(account),
    %insert(Record1, Info, "accounts"),
    update(Record2, Info, "accounts", "WHERE id = '"++Record2#account.id++"'").
        
test3() ->
    ok.