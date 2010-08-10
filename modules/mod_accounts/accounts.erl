%%--------------------------------------------------------------------
%% API and gen_server code for ganymede accounts system
%% 
%%--------------------------------------------------------------------

-module(accounts).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2]).
-export([handle_info/2, handle_call/3]).
-export([put/1, get/1, remove/1, update/2, exist/2]).
-export([test1/1]).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Exported Clients Functions
%% Operations & Maintance API 
%%
%%--------------------------------------------------------------------

start_link() ->
    start_link("accounts.cfg").

start_link(CfgFileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CfgFileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% Customer 
%% Operations & Maintance API 
%%
%%--------------------------------------------------------------------

put(Account) ->
    gen_server:call(?MODULE, {put, Account}).

get(ID) ->
    gen_server:call(?MODULE, {get, ID}).

remove(ID) ->
    gen_server:call(?MODULE, {remove, ID}).

update(ID, Account) ->
    gen_server:call(?MODULE, {update, ID, Account}).

exist(ID, Password) ->
     gen_server:call(?MODULE, {exist, ID, Password}).
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

handle_call({put, Account}, _From, State) ->
    {reply, accounts_db:put(Account), State};

handle_call({get, ID}, _From, State) ->
    {reply, accounts_db:get(ID), State};

handle_call({remove, ID}, _From, State) ->
    {reply, accounts_db:remove(ID), State};

handle_call({update, ID, Account}, _From, State) ->
    {reply, accounts_db:update(ID, Account), State};

handle_call({exist, ID, Password}, _From, State) ->
     {reply, accounts_db:exist(ID, Password), State}.

%%--------------------------------------------------------------------
%%
%% Tests
%%
%%--------------------------------------------------------------------

test1(N) ->
    Seq = lists:seq(1, N),
    F = fun(_) -> accounts:get("test_id_1") end,
    {_, T1, _} = now(),
    lists:foreach(F,Seq),
    {_, T2, _} = now(),
    T = T2-T1,
    io:format("TIME: ~p; RPS: ~p~n",[T, N]).