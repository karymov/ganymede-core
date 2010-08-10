%%--------------------------------------------------------------------
%% API and gen_server code for ganymede books metadatabase
%% 
%%--------------------------------------------------------------------

-module(metastore).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2]).
-export([handle_info/2, handle_call/3]).
-export([get_node/1, get_item/1, put/3, remove/1, update/3]).
-export([get_children/1, get_root/0, make_root/0, get_person/1, get_publisher/1]).

-behaviour(gen_server).

-include("ganymede.hrl").

%%--------------------------------------------------------------------
%% Exported Clients Functions
%% Operations & Maintance API 
%%
%%--------------------------------------------------------------------

start_link() ->
    start_link("metastore.cfg").

start_link(CfgFileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CfgFileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% Customer 
%% Operations & Maintance API 
%%
%%--------------------------------------------------------------------

put(DataNode, NodeItem, ParentID) ->
    gen_server:call(?MODULE, {put, DataNode, NodeItem, ParentID}).

get_node(ID) ->
    gen_server:call(?MODULE, {get_node, ID}).

get_item(ID) ->
    gen_server:call(?MODULE, {get_item, ID}).

remove(ID) ->
    gen_server:call(?MODULE, {remove, ID}).

update(ID, DataNode, NodeItem) ->
    gen_server:call(?MODULE, {update, ID, DataNode, NodeItem}).

get_children(ID) ->
     gen_server:call(?MODULE, {get_children, ID}).

get_root() ->
    gen_server:call(?MODULE, {get_root}).

make_root() ->
    gen_server:call(?MODULE, {make_root}).
    
get_person(ID) ->
    gen_server:call(?MODULE, {get_person, ID}).

get_publisher(ID) ->
    gen_server:call(?MODULE, {get_publisher, ID}).

%%--------------------------------------------------------------------
%% 
%% Callback Functions
%%
%%--------------------------------------------------------------------

init(_FileName) ->
    g_db:start_link(default),
    {ok, null}.

terminate(_Reason, _State) ->
    ok.

handle_cast(stop, State) ->
    {stop, normal, State}.
    
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call({put, DataNode, NodeItem, ParentID}, _From, State) ->
    {reply, metastore_db:put(DataNode, NodeItem, ParentID), State};

handle_call({get_node, ID}, _From, State) ->
    {reply, metastore_db:get_node(ID), State};

handle_call({get_item, ID}, _From, State) ->
    {reply, metastore_db:get_item(ID), State};

handle_call({remove, ID}, _From, State) ->
    {reply, metastore_db:remove(ID), State};

handle_call({update, ID, DataNode, NodeItem}, _From, State) ->
    {reply, metastore_db:update(ID, DataNode, NodeItem), State};

handle_call({get_children, ID}, _From, State) ->
    {reply, metastore_db:get_children(ID), State};

handle_call({get_root}, _From, State) ->
    {reply, metastore_db:get_root(), State};

handle_call({make_root}, _From, State) ->
    {reply, metastore_db:make_root(), State};

handle_call({get_person, ID}, _From, State) ->
    {reply, metastore_db:get_person(ID), State};

handle_call({get_publisher, ID}, _From, State) ->
    {reply, metastore_db:get_publisher(ID), State}.