%%--------------------------------------------------------------------
%% Accounts database implementation
%% 
%%
%%--------------------------------------------------------------------

-module(accounts_db).

-export([get/1]).
-export([put/1, remove/1, update/2, exist/2]).
-export([test1/0, test2/0, test3/0]).

-include("ganymede.hrl").

%%--------------------------------------------------------------------
%%
%% Module interface
%%
%%--------------------------------------------------------------------   

%% @doc
%% @spec
% get_all() ->
%     lists:map(
%         fun(Record) ->
%             list_to_account(Record)
%         end,
%         g_db:select("SELECT * FROM accounts")
%     ).

%% @doc
%% @spec
get(ID) when  is_list(ID) ->
    Query = "SELECT * FROM accounts WHERE id = '" ++ g_db:to_str(ID) ++ "'",
    case g_db:select(Query) of
        [] ->
            undefined;
        [Record] ->
            list_to_account(Record);
        Error ->
            {error, Error}
    end.

%% @doc
%% @spec
put(#account{} = Account) when erlang:is_record(Account, account) ->
    IdAndPassNotAreNull =
        g_db:not_null(Account#account.id) andalso
        g_db:not_null(Account#account.password),
    
    if IdAndPassNotAreNull
        %% if id and password is not null
         ->
            %% if id is unique
            case ?MODULE:get(Account#account.id) of
                undefined ->
                    Info = g_record_info(account),
                    HashedPassword = g_db:hash_str(Account#account.password),
                    case g_db:insert(Account#account{password = HashedPassword}, Info, "accounts") of
                        {ok, 1} ->
                            {ok, put};
                        DbError ->
                            {error, DbError}
                    end;
                _Record ->
                    {error, duplicate}
            end;
        true ->
            {error, empty}
    end.

%% @doc
%% @spec
remove(ID) ->
    Query = "DELETE FROM accounts WHERE id = '" ++ g_db:to_str(ID) ++ "'",
    g_db:delete(Query).
    
%% @doc
%% @spec
update(ID, #account{} = Account) when (Account#account.id =:= ID) ->
    Info = g_record_info(account),
    g_db:update(Account, Info, "accounts", "WHERE id = '" ++ g_db:to_str(ID) ++ "'").

%% @doc
%% @spec
exist(ID, Password) ->
    case ?MODULE:get(ID) of
        undefined ->
            {error, undefined};
        Account ->
            HashedPassword = list_to_binary(g_db:hash_str(Password)),
            if
                HashedPassword =:= Account#account.password ->
                    {ok, Account};
                true ->
                    {error, bad_password}
            end
    end.

%%--------------------------------------------------------------------
%%
%% Module Utilities
%%
%%--------------------------------------------------------------------

%% @doc convert PgSql List to #account record
%% @spec list_to_account([Field1, Field2, ...]) -> #account{}
list_to_account({ID,
                PersonID,
                Password,
                Name,
                Surname,
                Role,
                State,
                RegDatetime,
                LoginDatetime,
                Description,
                Email}) ->
                    
    #account{id = ID,
            person_id = g_db:if_null_to_undf(PersonID),
            password = g_db:if_null_to_undf(Password),
            name = g_db:if_null_to_undf(Name),
            surname = g_db:if_null_to_undf(Surname),
            description = g_db:if_null_to_undf(Description),
            reg_datetime = g_db:if_null_to_undf(RegDatetime),
            role = g_db:if_null_to_undf(Role),
            state = g_db:if_null_to_undf(State),
            login_datetime = g_db:if_null_to_undf(LoginDatetime),
            email = g_db:if_null_to_undf(Email)}.

%%--------------------------------------------------------------------
%%
%% Tests
%%
%%--------------------------------------------------------------------

test1() ->
    R = #account{ id = "test1_id",
        name = "test1_name",
        role = 1,
        login_datetime = <<"2010-06-25 13:57:21">>},
    put(R),
    ok.

test2() ->
    ok.

test3() ->
    ok.
