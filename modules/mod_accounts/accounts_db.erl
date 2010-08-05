%%--------------------------------------------------------------------
%% Accounts database implementation
%% 
%%
%%--------------------------------------------------------------------

-module(accounts_db).

-export([get/1]).
-export([put/1, remove/1, update/2, exist/2]).
%-export([test1/0, test2/0, test3/0]).

-include("ganymede.hrl").
-include("ganymede_utils.hrl").

%%--------------------------------------------------------------------
%%
%% Module interface
%%
%%--------------------------------------------------------------------   

get(ID) ->
    sql:select(account, ID).

%% @doc
%% @spec
put(#account{} = Account) when erlang:is_record(Account, account) ->
    case field_length(Account#account.password) > 0 of
        true ->
            case login_exist(Account#account.login, {return, null}) of
                false ->
                    EncodePassword = encode_password(Account#account.password),
                    case sql:insert(Account#account{password = EncodePassword}) of
                        {ok, ID} ->
                            {ok, put};
                        DbError ->
                            {error, DbError}
                    end;
                true ->
                    {error, dublicate}
            end;
        false ->
            {error, bad_password}
    end;
put(_) ->
    {error, empty}.

%% @doc
%% @spec
remove(ID) ->
    sql:delete(account, ID).
    
%% @doc
%% @spec
update(ID, #account{} = Account) when (Account#account.id =:= ID) ->
    {error, not_implemented}.

%% @doc
%% @spec
exist(Login, Password) ->
    case login_exist(Login, {return, id}) of
        {false, undefined} ->
            {error, undefined};
        {true, ID} ->
            Account = ?MODULE:get(ID),
            EncodedPassword = to_bin(encode_password(Password)),
            case EncodedPassword =:= to_bin(Account#account.password) of
                true ->
                    {ok, Account};
                false ->
                    {error, bad_password}
            end
    end.

%%--------------------------------------------------------------------
%%
%% Module Utilities
%%
%%--------------------------------------------------------------------
field_length(Password) when is_list(Password) ->
    length(Password);
field_length(Password) when is_binary(Password) ->
    length(list_to_binary(Password));
field_length(_) ->
    0.

login_exist(Login, {return, null}) ->
    case sql:equery("SELECT login FROM accounts WHERE login=$1",[Login]) of
        {ok, _, []} ->
            false;
        {ok, _, _} ->
            true
    end;
login_exist(Login, {return, id}) ->
    case sql:equery("SELECT id FROM accounts WHERE login=$1",[Login]) of
        {ok, _, []} ->
            {false, undefined};
        {ok, _, [{ID}]} ->
            {true, ID}
    end.

encode_password(Password) ->
    Bytes = crypto:sha(Password),
    List = binary_to_list(Bytes),
    lists:flatten(list_to_hex(List)).