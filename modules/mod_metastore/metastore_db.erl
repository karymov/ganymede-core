%%--------------------------------------------------------------------
%% Metastore database implementation
%% 
%%
%%--------------------------------------------------------------------

-module(metastore_db).

-export([get_node/1, get_item/1, put/3, remove/1, update/3]).
-export([get_children/1, get_root/0, make_root/0]).

-include("ganymede.hrl").

%%--------------------------------------------------------------------
%%
%% Module interface
%%
%%--------------------------------------------------------------------

get_node(ID) ->
    sql:select(data_node, ID).

get_item(ID) ->
    sql:select(get_node_atom(ID), ID).
    
put(#data_node{} = Node, Item, ParentID) when is_integer(ParentID) ->
    case node_exist(ParentID) of
        true ->
            {ok, ID} = sql:insert(
                Node#data_node{
                    name = element(3, Item),
                    parent = ParentID,
                    type = node_type(Node#data_node.type)
                }),
            sql:insert2(Item, ID);
        false ->
            {error, null_parent}
    end.
        
remove(ID) ->
    Type = get_node_atom(ID),
    sql:delete(data_node, ID),
    sql:delete(Type, ID).
    
update(ID, #data_node{} = Node, NodeItem) ->
    case node_exist(ID) of
        true ->
            sql:update(ID, Node),
            sql:update(ID, NodeItem);
        false ->
            {error, undefined}
    end.

get_children(ID) ->
    children(ID).

get_root() ->
    get_node(0).
    
make_root() ->
    Root = #data_node{
                        name = "root",
                        type = node_type(category)
                    },
    sql:insert2(Root, 0).
    
%%--------------------------------------------------------------------
%%
%% Module Utilities
%%
%%--------------------------------------------------------------------

node_exist(ID) ->
    case sql:equery("SELECT id FROM data_nodes WHERE id=$1",[ID]) of
        {ok, _, []} ->
            false;
        {ok, _, _} ->
            true
    end.

get_node_atom(ID) ->
    case sql:equery("SELECT type FROM data_nodes WHERE id=$1",[ID]) of
        {ok, _, [{Type}]} ->
            node_atom(Type);
        X ->
            io:format("~p~n",[X]),
            undefined
    end.

children(ID) ->
    case sql:equery("SELECT id,name,type FROM data_nodes WHERE parent=$1",[ID]) of
        {ok, _, []} ->
            undefined;
        {ok, _, Rows} ->
           Rows
        end.         