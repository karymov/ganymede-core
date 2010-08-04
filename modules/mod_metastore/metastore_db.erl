%%--------------------------------------------------------------------
%% Metastore database implementation
%% 
%%
%%--------------------------------------------------------------------

-module(metastore_db).

-export([get_node/1,  get_item/1, put/3, remove/1, update/3, exist/1]).
-export([make_root/0, get_root/0]).
-export([test1/0, test2/0, test3/0]).

-include("ganymede.hrl").

%%--------------------------------------------------------------------
%%
%% Module interface
%%
%%--------------------------------------------------------------------

%% @doc
%% @spec
get_node(ID) ->
    Query = "SELECT * FROM data_nodes WHERE id = '" ++ g_db:to_str(ID) ++ "'",
    get_record(Query, fun list_to_data_node/1).

%% @doc
%% @spec
get_item(ID) ->
    case ?MODULE:exist(ID) of
        {ok, Node} ->
            {Query, Deserializer} = case Node#data_node.type of
                book ->
                    {"SELECT * FROM book_metas WHERE id = '" ++ g_db:to_str(ID) ++ "'",
                    fun list_to_book_meta/1};
                category ->
                    {"SELECT * FROM category_metas WHERE id = '" ++ g_db:to_str(ID) ++ "'",
                    fun list_to_category_meta/1};
                _OtherType ->
                    {error, unknown_item_type}
            end,
            get_record(Query, Deserializer);
        {error, undefined} ->
            undefined
    end.

%% @doc
%% @spec
put_node(DataNode) ->
        %% if id don't exist
        case ?MODULE:exist(DataNode#data_node.id) of
            {error, undefined} ->
                %% get g_record_type of node
                DataNodeRecordInfo = g_record_info(data_node),
                %% put node
                case g_db:insert(DataNode, DataNodeRecordInfo, "data_nodes") of
                    {ok, 1} ->
                        {ok, put};
                    DbError ->
                        {error, DbError}
                end;
            {ok, _Record} ->
                {error, duplicate}
        end.

%% @doc
%% @spec
put_item(_Item, undefined) ->
    ok;
put_item(undefined, _DataNode) ->
    ok;
put_item(Item, DataNode) ->
    case DataNode#data_node.type of
        book ->
            g_db:insert(Item#book_meta{id = DataNode#data_node.id},
                g_record_info(book_meta),
                "book_metas");
        category ->
            g_db:insert(Item#category_meta{id = DataNode#data_node.id},
            g_record_info(category_meta),
            "category_metas")
    end.
    

%% @doc
%% @spec
put(#data_node{} = DataNode, NodeItem, ParentID) when ParentID =/= undefined ->
    ID = gen_id(),
    case  ?MODULE:exist(ParentID) of
        {ok, ParentNode} ->
            Node = DataNode#data_node{id = ID, parent = ParentID},
            case put_node(Node) of
                %% if it's ok -> update parent children list
                {ok, put} ->
                    %% if it's ok insert node's item
                    put_item(NodeItem, Node),
                    UpdatedParent = ParentNode#data_node{ children = [ID| ParentNode#data_node.children]},
                    case ?MODULE:update(ParentID, UpdatedParent, undefined) of
                        {ok, 1} ->
                            {ok, put};
                        Error ->
                            {error, {parent_update_error, Error}}
                    end;
                Error ->
                    {error, {put_error, Error}}
            end;
        {error, undefined} ->
            {error, null_parent}
    end.
        

%% @doc
%% @spec
remove(ID) ->
    %% remove DataNode from data_nodes table
    Query = "DELETE FROM accounts WHERE id = '" ++ g_db:to_str(ID) ++ "'",
    g_db:delete(Query)
    %% remove DataNodeInfo
    .
    
%% @doc
%% @spec
update(ID, #data_node{} = DataNode, NodeItem) ->
    GRecordInfo = g_record_info(data_node),
    case g_db:to_str(DataNode#data_node.id) =:= g_db:to_str(ID) of
        true ->
            %% update data_node
            g_db:update(DataNode, GRecordInfo, "data_nodes", "WHERE id = '" ++ g_db:to_str(ID) ++ "'"),
            %% update data_node_info
            update_item(NodeItem, DataNode);
        false ->
            {error, illegal_id}
    end.

%% @doc
%% @spec
update_item(undefined, DataNode) ->
    ok;
update_item(Item, DataNode) ->
    ID = DataNode#data_node.id,
    case DataNode#data_node.type of
        book ->
            g_db:update(Item#book_meta{id = ID},
                g_record_info(book_meta),
                "book_metas",
                "WHERE id = '" ++ g_db:to_str(ID) ++ "'");
        category ->
            g_db:update(Item#category_meta{id = ID},
                g_record_info(category_meta),
                "category_metas",
                "WHERE id = '" ++ g_db:to_str(ID) ++ "'")
    end.
    
    

%% @doc
%% @spec
exist(ID) ->
    case ?MODULE:get_node(ID) of
        undefined ->
            {error, undefined};
        DataNode ->
            {ok, DataNode}
    end.

%% @doc
%% @spec
make_root() ->
    RootNode = #data_node{id = "root",
            name = "root node",
            type = category,
            parent = "",
            children = ""},
    put_node(RootNode).
    
%%--------------------------------------------------------------------
%%
%% Module Utilities
%%
%%--------------------------------------------------------------------

%% @doc
%% @spec
get_record(Query, Deserializer) ->
    case g_db:select(Query) of
        [] ->
            undefined;
        [Record] ->
            Deserializer(Record);
        Error ->
            {error, Error}
    end.

%% @doc convert PgSql List to #data_node record
%% @spec list_to_data_node({Field1, Field2, ...}) -> #data_node{}
list_to_data_node({ID,
                Name,
                Type,
                Parent,
                Children}) ->      
    #data_node{id = ID,
            name = g_db:if_null_to_undf(Name),
            type = g_db:parse_atom(Type),
            parent = g_db:if_null_to_undf(Parent),
            children = g_db:parse_str_list(Children)}.
            
list_to_book_meta({ID,
                Name,
                AuthorID,
                PublisherID,
                Year,
                PagesCount,
                Abstract,
                Discipline,
                ResourcesIDs,
                CoversIDs}) ->
    #book_meta{id = ID,
            name = g_db:if_null_to_undf(Name),
            author_id = g_db:if_null_to_undf(AuthorID),
            publisher_id = g_db:if_null_to_undf(PublisherID),
            year = g_db:if_null_to_undf(Year),
            pages_count = g_db:if_null_to_undf(PagesCount),
            abstract = g_db:if_null_to_undf(Abstract),
            discipline = g_db:if_null_to_undf(Discipline),
            resources_ids = g_db:parse_str_list(ResourcesIDs),
            covers_ids = g_db:parse_str_list(CoversIDs)}.
    
list_to_category_meta({ID,
                Name,
                Discipline,
                Description,
                ResourcesIDs,
                CoversIDs}) ->
   #category_meta{id = ID,
            name = g_db:if_null_to_undf(Name),
            discipline = g_db:if_null_to_undf(Discipline),
            description = g_db:if_null_to_undf(Description),
            resources_ids = g_db:parse_str_list(ResourcesIDs),
            covers_ids = g_db:parse_str_list(CoversIDs)}.

%% @doc
%% @spec
gen_id() ->
    {A, B, C} = now(),
    String = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
    ID = g_db:hash_str(String),
    case ?MODULE:exist(ID) of
        {error, undefined} -> ID;
        {ok, _Data} -> gen_id()
    end.

%% @doc
%% @spec
get_root() ->
    ?MODULE:get_node("root").

%%--------------------------------------------------------------------
%%
%% Tests
%%
%%--------------------------------------------------------------------

test1() ->
    Test = #data_node{ id = "c9", name = "c9", type = book},
    ?MODULE:put(Test, undefined, "root").

test2() ->
    ok.

test3() ->
    ok.
