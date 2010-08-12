%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accounts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author     Vladimir Zaytsev <vladimir.zaytsev.m@gmail.com>
%%% @copyright  2010 Vladimir Zaytsev
%%% @doc  
%%% @reference
%%% @end
%%%
%%% X: License???
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(accounts).

-export([start_link/0]).

-export([put/1, get/1, remove/1, update/3, exist/2]).

-export([init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-behaviour(gen_server).

-include("ganymede.hrl").
-include("ganymede_utils.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec put(Account) -> {ok, ID} | {error, Reason}
%%  Account = #account{}
%% @end
put(Account) ->
    gen_server:call(?MODULE, {put, Account}).

%% @spec get(ID) -> #account{} | undefined
%%  ID = integer
%% @end
get(ID) ->
    gen_server:call(?MODULE, {get, ID}).

%% @spec remove(ID) -> {ok, ID} | {error, Reason}
%%  ID = integer
%% @end
remove(ID) ->
    gen_server:call(?MODULE, {remove, ID}).

%% @spec update(ID, Account) -> {ok, ID} | {error, Reason}
%%  ID = integer
%%  Account = #account{}
%%  Password = list | binary
%% @end
update(ID, Account, Password) ->
    gen_server:call(?MODULE, {update, ID, Account, Password}).

%% @spec exist(Login, Password) -> {ok, Account} | {error, Reason}
%%  Login = list | binary
%%  Password = list | binary
%%  Account = #account{}
%% @end
exist(Login, Password) ->
     gen_server:call(?MODULE, {exist, Login, Password}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, []}.
    
handle_call({put, Account}, _From, State) -> {reply, put_account(Account), State};

handle_call({get, ID}, _From, State) -> {reply, get_account(ID), State};

handle_call({remove, ID}, _From, State) -> {reply, remove_account(ID), State};

handle_call({update, ID, Account, Password}, _From, State) -> {reply, update_account(ID, Account, Password), State};

handle_call({exist, Login, Password}, _From, State) -> {reply, login_exist(Login, Password), State}.

handle_cast(_, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_account(#account{} = Account) ->
    case field_length(Account#account.password) > 0 of
        true ->
            case login_id(Account#account.login) of
                {error, undefined} ->
                    EncodePassword = encode_password(Account#account.password),
                    case sql:insert(Account#account{password = EncodePassword}) of
                        {ok, ID} -> {ok, put};
                        {error, Reason} -> {error, Reason}
                    end;
                {ok, _} -> {error, duplicate}
            end;
        false -> {error, empty_password}
    end;
put_account(_) -> {error, bad_record}.
    
get_account(ID) -> sql:select(account, ID).
        
remove_account(ID) ->
    case sql:delete(account, ID) of
        {ok, 1} -> {ok, ID};
        {ok, 0} -> {error, undefined};
        {error, Reason} -> {error, Reason}
    end.
    
update_account(ID, #account{} = Account, Password) ->
    case login_exist(Account#account.login, Password) of
        {ok, ID} when ID =:= Account#account.id ->
            sql:update(Account),
            {ok, ID};
        {error, Reason} -> {error, Reason}
    end;
update_account(_, _, _) -> {error, bad_record}.

login_exist(Login, Password) ->
    case login_id(Login) of
        {error, undefined} -> {error, undefined};
        {ok, ID} ->
            Account = get_account(ID),
            EncodedPassword = to_bin(encode_password(Password)),
            case EncodedPassword =:= to_bin(Account#account.password) of
                true -> {ok, Account};
                false -> {error, bad_password}
            end
    end.

field_length(Password) when is_list(Password) ->
    length(Password);
field_length(Password) when is_binary(Password) ->
    length(list_to_binary(Password));
field_length(_) ->
    0.

login_id(Login) ->
    case sql:equery("SELECT id FROM accounts WHERE login=$1",[Login]) of
        {ok, _, []} -> {error, undefined};
        {ok, _, [ID]} -> {ok, ID}
    end.

encode_password(Password) ->
    Bytes = crypto:sha(Password),
    List = binary_to_list(Bytes),
    lists:flatten(list_to_hex(List)).