%%--------------------------------------------------------------------
%% Ganymede
%%--------------------------------------------------------------------

-export([g_record_info/1, record_types/1]).

%%--------------------------------------------------------------------
%%
%% Records
%%
%%--------------------------------------------------------------------

%% @doc
-record(g_record_info,
    {info
}).

%% @doc
-record(account,
    {id, person_id,
    password, name, surname,
    description, reg_datetime,
    login_datetime, role, state, email
}).

%% @doc
-record(data_node,
    {id, name, type, parent, children}).

%% @doc
-record(book_meta,
    {id, name, author_id,
    publisher_id, year, pages_count,
    abstract, discipline,
    resources_ids, covers_ids}).

%% @doc
-record(category_meta,
    {id, name, discipline,
    description, resources_ids, covers_ids}).

%% @doc
-record(person_meta,
    {id, name, surname}).

%% @doc
-record(resource_meta,
    {id, name, filepath, type_id}).
%%--------------------------------------------------------------------
%%
%% g_record_info
%%
%%--------------------------------------------------------------------
g_record_info(Info, Types) ->
    #g_record_info{
        info = 
        lists:zipwith(
            fun(Field, Type) ->
                {Field, Type}
            end,
            Info,
            Types
        )
    }.

g_record_info(account) ->
    g_record_info( record_info(fields, account), record_types(account));

g_record_info(data_node) ->
    g_record_info( record_info(fields, data_node), record_types(data_node));

g_record_info(book_meta) ->
    g_record_info( record_info(fields, book_meta), record_types(book_meta));

g_record_info(category_meta) ->
    g_record_info( record_info(fields, category_meta), record_types(category_meta)).   
%%--------------------------------------------------------------------
%%
%% record_types
%%
%%--------------------------------------------------------------------

record_types(account) ->
    [str, str, str, str,
    str, str, datetime,
    datetime, int, int,
    str];

record_types(data_node) ->
    [str, str, str, str,
    str_list];

record_types(book_meta) ->
    [str, str, str, str,
    int, int, str, str,
    str_list, str_list];

 record_types(category_meta) ->
     [str, str, str, str,
     str_list, str_list].

%%--------------------------------------------------------------------
%%
%% UTILITIES
%%
%%--------------------------------------------------------------------

%% @doc
%% @spec
role_atom(N) when is_binary(N) -> role_atom(list_to_integer(binary_to_list(N)));
role_atom(0) -> visitor;
role_atom(1) -> user;
role_atom(2) -> admin;
role_atom(3) -> moderator;
role_atom(_) -> visitor.

%% @doc
%% @spec
atom_role(visitor) -> 0;
atom_role(user) -> 1;
atom_role(admin) -> 2;
atom_role(moderator) -> 3;
atom_role(_) -> 0.