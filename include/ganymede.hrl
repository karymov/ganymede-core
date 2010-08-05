%%--------------------------------------------------------------------
%% Ganymede
%%--------------------------------------------------------------------

-record(account,
    {id = null,
    login = null,
    person_id = null,
    password = null,
    name = null,
    surname = null,
    email = null,
    description = null,
    role = 0,
    state = 0,
    reg_datetime = null,
    login_datetime = null
}).

-record(data_node,
    {id = null,
    name = null,
    type = 0,
    parent = null,
    children = null}).

-record(book_meta,
    {id = null,
    name = null,
    author_id = null,
    publisher_id = null,
    year = null,
    pages_count = null,
    abstract = null,
    discipline = null,
    resources_ids = null,
    covers_ids = null}).

-record(category_meta,
    {id = null,
    name = null,
    discipline = null,
    description = null,
    resources_ids = null,
    covers_ids = null}).

-record(person_meta,
    {id, name, surname}).

-record(resource_meta,
    {id, name, filepath, type_id}).

%% DELETE BEGIN 

-record(g_record_info,
    {info
}).

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

record_fields(account) ->
    record_info(fields, account);
record_fields(data_node) ->
    record_info(fields, data_node);
record_fields(book_meta) ->
    record_info(fields, book_meta);
record_fields(category_meta) ->
    record_info(fields, category_meta).

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
% DELETE END 

role_atom(N) when is_binary(N) -> role_atom(list_to_integer(binary_to_list(N)));
role_atom(0) -> visitor;
role_atom(1) -> user;
role_atom(2) -> admin;
role_atom(3) -> moderator;
role_atom(_) -> visitor.

role_type(visitor) -> 0;
role_type(user) -> 1;
role_type(admin) -> 2;
role_type(moderator) -> 3;
role_type(_) -> 0.

data_node_type(category) -> 0;
data_node_type(book) -> 1.

data_node_atom(0) -> category;
data_node_atom(1) -> book.
