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
    login_datetime = null}).

-record(data_node,
    {id = null,
    name = null,
    type = 0,
    parent = null}).

-record(book_meta,
    {id = null,
    name = null,
    author_id = null,
    publisher_id = null,
    year = null,
    pages_count = null,
    abstract = null,
    discipline = null}).

-record(category_meta,
    {id = null,
    name = null,
    discipline = null,
    description = null}).

-record(person_meta,
    {id,
    name,
    surname}).

-record(resource_meta,
    {id,
    name,
    filepath,
    type_id}).

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

node_atom(Atom) when is_atom(Atom) -> Atom; 
node_atom(0) -> category_meta;
node_atom(1) -> book_meta.

node_type(Integer) when is_integer(Integer) -> Integer;
node_type(category) -> 0;
node_type(book) -> 1.