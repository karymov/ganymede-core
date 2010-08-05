%%--------------------------------------------------------------------
%% API and gen_server code for ganymede books metadatabase
%% 
%%--------------------------------------------------------------------

-module(metastore).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2]).
-export([handle_info/2, handle_call/3]).
-export([get_node/1, get_item/1, put/3, remove/1, update/3]).
-export([get_children/1, get_root/0, make_root/0]).

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
    

%%--------------------------------------------------------------------
%% 
%% Callback Functions
%%
%%--------------------------------------------------------------------

init(_FileName) ->
    g_db:start_link(default),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.
    
handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.

handle_call({put, DataNode, NodeItem, ParentID}, _From, LoopData) ->
    {reply, metastore_db:put(DataNode, NodeItem, ParentID), LoopData};

handle_call({get_node, ID}, _From, LoopData) ->
    {reply, metastore_db:get_node(ID), LoopData};

handle_call({get_item, ID}, _From, LoopData) ->
    {reply, metastore_db:get_item(ID), LoopData};

handle_call({remove, ID}, _From, LoopData) ->
    {reply, metastore_db:remove(ID), LoopData};

handle_call({update, ID, DataNode, NodeItem}, _From, LoopData) ->
    {reply, metastore_db:update(ID, DataNode, NodeItem), LoopData};

handle_call({get_children, ID}, _From, LoopData) ->
    {reply, metastore_db:get_children(ID), LoopData};

handle_call({get_root}, _From, LoopData) ->
    {reply, metastore_db:get_root(), LoopData};

handle_call({make_root}, _From, LoopData) ->
    {reply, metastore_db:make_root(), LoopData}.