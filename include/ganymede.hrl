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

-record(book,
	{id=null,
	name=null,
	authors=[],
	description=null,
	year=null,
	disciplines=[],
	url=null,
	cover=null,
	pages_count=null,
	files=[],
	publishers=[]}).
-record(video,
	{id=null,
	name=null,
	authors=[],
	description=[],
	year=null,
	disciplines=[],
	url=null,
	cover=null,
	files=[],
	publishers=[],
	format=null}).
-record(audio,
	{id=null,
	name=null,
	authors=[],
	description=null,
	year=null,
	disciplines=[],
	url=null,
	cover=null,
	files=[],
	publishers=[],
	format=null}).
-record(presentation,
	{id=null,
	name=null,
	authors=[],
	description=null,
	year=null,
	disciplines=[],
	url=null,
	cover=null,
	slides_count=null,
	files=[],
	publishers=[]}).

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
    duration = null,
    file = null,
	tag = null}).

-record(person,
    {id = null,
    name = null,
    phone = null,
    email = null,
    site = null,
    adress = null,
    bio = null}).

-record(discipline,
	{id = null,
	name = null}).

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