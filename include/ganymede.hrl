%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ganymede_header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author     Vladimir Zaytsev <vladimir.zaytsev.m@gmail.com>
%%% @copyright  2010 Vladimir Zaytsev
%%% @doc  
%%% @reference
%%% @end
%%%
%%% X: License???
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-record(node,
    {id = null,
    name = null,
    type = 0,
    parent = null}).
    
-record(category_meta,
    {id = null,
    name = null,
    discipline = null,
    description = null}).

-record(rsc_meta,
    {id = null,
    name = null,
    type = null,
    author = null,
    description = null,
    year = null,
    discipline = null,
    url = null,
    cover = null,
    pages_count = null,
    size = null,
    publisher = null,
    duration = null}).

-record(person,
    {id = null,
    name = null,
    surname = null,
    phone = null,
    email = null,
    site = null,
    adress = null,
    bio = null}).

-record(publisher,
    {id = null,
    name = null,
    fullname = null,
    email = null,
    site = null,
    logo_id = null,
    descritpion = null}).
    
-record(fileinfo,
    {id = null,
    name = null,
    extention = null,
    format = null,
    duration = null,
    size = null}).

-record(cover,
    {id = null,
    name = null,
    file = null}).

-record(tag,
    {id = null,
    name = null,
    resource_id = null}).
    
-record(json_book,{name,authors,description,year,url,cover,pages_count,files,publishers}).
-record(json_video,{name,authors,description,year,url,cover,files,publishers,format}).
-record(json_audio,{name,authors,description,year,url,cover,files,publishers,format}).
-record(json_presentation,{name,authors,description,year,url,cover,pages_count,files,publishers}).


node_type(category) -> 0;
node_type(book) -> 1;
node_type(video) -> 2;
node_type(audio) -> 3;
node_type(presentation) -> 4.

node_type_atom(0) -> category;
node_type_atom(1) -> book;
node_type_atom(2) -> video;
node_type_atom(3) -> audio;
node_type_atom(4) -> presentation.

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