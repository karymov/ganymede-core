%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta_tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author     Vladimir Zaytsev <vladimir.zaytsev.m@gmail.com>
%%% @copyright  2010 Vladimir Zaytsev
%%% @doc  
%%% @reference
%%% @end
%%%
%%% X: License???
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(meta_tree).
-behavior(gen_server).

-export([start_link/0]).

-export([add/2, get/1, remove/1]).
-export([update/2, replace/2]).
-export([children/1, root_id/0, make_root/0]).
-export([monitor/0]).

-export([init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-include("ganymede.hrl").

-define(ROOT_ID, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
%% @spec add(Meta, ParentID) -> {ok, ID} | {error, Reason}
%%  Meta = #category_meta{} | #rsc_meta{}
%%  ParentID = integer
%% @end
add(Meta, ParentID) ->
    gen_server:call(?MODULE, {add, Meta, ParentID}).

%% @spec get(ID) -> #category_meta{} | #rsc_meta{} | undefined
%%  ID = integer
%% @end
get(ID) ->
    gen_server:call(?MODULE, {get, ID}).

%% @spec remove(ID) -> {ok, ID} | undefined
%%  ID = integer
%% @end
remove(ID) ->
    gen_server:call(?MODULE, {remove, ID}).

%% @spec update(ID, Meta) -> {ok, ID} | {error, Reason}
%%  ID = integer
%%  Meta = #category_meta{} | #rsc_meta{}
%% @end
update(ID, Meta) ->
    gen_server:call(?MODULE, {update, ID, Meta}).

%% @spec replace(ID, ParentID) -> {ok, ID} | {error, Reason}
%%  ID = integer
%%  NewParentID = integer
%% @end
replace(ID, NewParentID) ->
    gen_server:call(?MODULE, {replace, ID, NewParentID}).

%% @spec children(ID) -> [{ID,Name,Type},...] | {error, Reason}
%%  ID = integer
%% @end
children(ID) ->
    gen_server:call(?MODULE, {children, ID}).

%% @spec root_id() -> ID | undefined
%% @end
root_id() ->
    gen_server:call(?MODULE, root).

%% @spec make_root() -> {ok, ID} | {error, Reason}
%% @end
make_root() ->
    gen_server:call(?MODULE, make_root).

%% @spec monitor() -> ok
%% @end
monitor() ->
    gen_server:call(?MODULE, monitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
    {ok, []}.
    
handle_call({add, Meta, ParentID}, _From, State) ->
    R = case get_node_type(ParentID) of
        {ok, category} -> add_node(Meta, ParentID);
        {ok, _} -> {error, bad_parent_type};
        {error, undefined} -> {error, undefined_parent}
    end, {reply, R, State};

handle_call({get, ID}, _From, State) ->
    R = case get_node_type(ID) of
        {ok, category} -> sql:select(node, ID);
        {ok, _} ->  sql:select(rsc_meta, ID);
        {error, _} -> undefined
    end, {reply, R, State};
    
handle_call({remove, ID}, _From, State) ->
   R = case get_node_type(ID) of
        {ok, category} ->
            case ?MODULE:children(ID) of
                [] ->
                    case sql:select(node, ID) of
                        {ok, 1} -> {ok, ID};
                        {ok, 0} -> {error, undefined};
                        Error -> {error, Error}
                    end;
                _ -> {error, category_not_empty}
            end;
        {ok, _} ->
            case sql:select(rsc_meta, ID) of
                {ok, 1} -> {ok, ID};
                {ok, 0} -> {undefined};
                Error -> {error, Error}
            end;
        {error, _} -> {error, undefined}
    end, {reply, R, State};

handle_call({update, ID, Meta}, _From, State) ->
    R = update_node(ID, Meta),
    {reply, R, State};

handle_call({replace, ID, NewParentID}, _From, State) ->
    R = case get_node_type(NewParentID) of
        {ok, category} -> replace_node(ID, NewParentID);
        {ok, _} -> {error, bad_new_parent_type};
        {error, undefined} -> {error, undefined_new_parent}
    end, {reply, R, State};
    
handle_call({children, ID}, _From, State) ->
    R = case get_node_type(ID) of
        {ok, category} -> get_children(ID);
        {ok, _} -> {error, bad_parent_type};
        {error, undefined} -> {error, undefined_parent}
    end, {reply, R, State};

handle_call(root_id, _From, State) ->
    {reply, ?ROOT_ID, State};
    
handle_call(make_root, _From, State) ->
    {ok, _} = sql:insert2(#node{name = <<"root">> },?ROOT_ID),
    {reply, {ok, ?ROOT_ID}, State};
    
handle_call(monitor, _From, State) ->
    {reply, {ok, monitor}, State}.
    
handle_cast(_, State) -> {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_node_type(ID) ->
    case sql:equery("SELECT type FROM nodes WHERE id=$1",[ID]) of
        {ok, _, [{Type}]} -> {ok, node_type_atom(Type)};
        _ -> {error, undefined}
    end.

add_node(Meta = #rsc_meta{}, ParentID) when Meta#rsc_meta.type =:= 0 ->
    {error, bad_rsc_meta_type};
add_node(Meta = #rsc_meta{}, ParentID) ->
    Node = #node{name=Meta#rsc_meta.name,
        type=Meta#rsc_meta.type,
        parent=ParentID},
    {ok, ID} = sql:insert(Node),
    {ok, ID} = sql:insert2(Meta,ID),
    {ok, ID};
add_node(Meta = #category_meta{}, ParentID) ->
    Node = #node{name=Meta#category_meta.name,
        type=node_type(category),
        parent=ParentID},
    {ok, ID} = sql:insert(Node),
    {ok, ID} = sql:insert2(Meta,ID),
    {ok, ID};
add_node(_, _) -> {error, bad_meta_type}.

update_node(ID, Meta = #rsc_meta{}) ->
    case sql:select(rsc_meta, ID) of
        undefined -> {error, undefined};
        OldMeta when OldMeta#rsc_meta.type =:= Meta#rsc_meta.type ->
            NodeName = Meta#rsc_meta.name,
            {ok, 1} = sql:equery("UPDATE nodes SET name=$1 WHERE id=$2",[NodeName,ID]),
            {ok, 1} = sql:update(Meta), {ok, ID};
        OldMeta -> {error, bad_rsc_meta_type}
    end;
update_node(ID, Meta = #category_meta{}) ->
    NodeName = Meta#category_meta.name,
    {ok, 1} = sql:equery("UPDATE nodes SET name=$1 WHERE id=$2",[NodeName,ID]),
    {ok, 1} = sql:update(Meta), {ok, ID}.

replace_node(ID, NewParentID) ->
    {ok, 1} = sql:equery("UPDATE nodes SET parent=$1 WHERE id=$2",[NewParentID, ID]),
    {ok, ID}.
    
get_children(ID) ->
    {ok, _, Rows} = sql:equery("SELECT id,name,type FROM nodes WHERE parent=$1",[ID]),
    Rows.