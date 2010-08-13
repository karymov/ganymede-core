%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta_db
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author     Vladimir Zaytsev <vladimir.zaytsev.m@gmail.com>
%%% @copyright  2010 Vladimir Zaytsev
%%% @doc  
%%% @reference
%%% @end
%%%
%%% X: License???
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(meta_db).

-behavior(gen_server).

-export([start_link/0]).

-export([add_book/2, add_video/2, add_audio/2, add_presentation/2]).
-export([add_person/0, add_publisher/0, add_fileinfo/0, add_cover/0]).
-export([add_discipline/0, add_tag/0, monitor/0]).

-export([init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-include("ganymede.hrl").
-include("ganymede_hacks.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec add_book(CategoryID, Book) -> {ok, ID} | {error, Reason}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_book(CategoryID, Book) ->
    gen_server:call(?MODULE, {add_book, CategoryID, Book}).

%% @spec add_video(CategoryID, Video) -> {ok, ID} {error, Reason}
%%  CategoryID = integer
%%  Video = #video
%% @end
add_video(CategoryID, Video) ->
    gen_server:call(?MODULE, {add_video, CategoryID, Video}).

%% @spec add_book(CategoryID, Audio) -> {ok, ID} {error, Reason}
%%  CategoryID = integer
%%  Audio = #audio
%% @end
add_audio(CategoryID, Audio) ->
    gen_server:call(?MODULE, {add_audio, CategoryID, Audio}).

%% @spec add_presentation(CategoryID, Presentation) -> {ok, ID} {error, Reason}
%%  CategoryID = integer
%%  Presentation = #presentation
%% @end
add_presentation(CategoryID, Presentation) ->
    gen_server:call(?MODULE, {add_presentation, CategoryID, Presentation}).
    
%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_person() ->
    ok.
    
get_person(ID) ->
    ok.

%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_publisher() ->
    ok.

get_publiser(ID) ->
    ok.

%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_fileinfo() ->
    ok.
    
get_fileinfo() ->
    ok.
    
%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_cover() ->
    ok.
    
get_cover() ->
    ok.
    
%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_discipline() ->
    ok.
    
get_discipline() ->
    ok.
    
%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
add_tag() ->
    ok.
    
get_tag() ->
    ok.

%% @spec get_rsc(ID) -> #src_meta{} | undefined
%%  ID = integer
%% @end
get_rsc(ID) ->
    gen_server:call(?MODULE, {get_src, ID}).

%% @spec add_book(CategoryID, Book) -> {ok, ID}
%%  CategoryID = integer
%%  Book = #book
%% @end
monitor() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
    {ok, []}.

handle_call({add_book, CategoryID, Book}, _From, State) ->
    RSC = #rsc_meta{
        name = Book#book.name,
        type = node_type(book),
        description = Book#book.description,
        year = Book#book.year,
        url = Book#book.url,
        cover = Book#book.cover,
        pages_count = Book#book.pages_count
    },
	R = case meta_tree:add(RSC, CategoryID) of
		{ok, ID} ->
			add_authors_to_rsc(ID, Book#book.authors),
			add_publishers_to_rsc(ID, Book#book.publishers),
			add_disciplines_to_rsc(ID, Book#book.disciplines),
			add_files_to_rsc(ID, Book#book.files),
			commit_rsc(ID);
		Error -> {error, Error}
	end, {reply, R, State};

handle_call({add_video, CategoryID, Video}, _From, State) ->
    {reply, ok, State};

handle_call({add_audio, CategoryID, Audio}, _From, State) ->
    {reply, ok, State};

handle_call({add_presentation, CategoryID, Presentation}, _From, State) ->
    {reply, ok, State};

handle_call({get_src, ID}, _From, State) ->
    {reply, meta_tree:get(ID), State}.
    
handle_cast(_, State) -> {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commit_rsc(ID) ->
	Meta = meta_tree:get(ID),
	RSC = case node_type_atom(Meta#rsc_meta.type) of
		category ->
			Meta#rsc_meta{
				discipline=get_disciplines(ID)
			};
		_ ->
			Meta#rsc_meta{
				author=get_authors(ID),
				discipline=get_disciplines(ID),
				publisher=get_publishers(ID),
				file=get_files(ID)}
	end,
	meta_tree:update(ID, RSC).	

get_authors(ID) ->
	SQL1 = "SELECT author_id FROM rsc_author WHERE rsc_id=$1",
	SQL2 = "SELECT name FROM people WHERE id=$1",
	Names = many_to_many(SQL1, SQL2, ID),
	enumerate_bin_list(Names).

get_publishers(ID) ->
    SQL1 = "SELECT publisher_id FROM rsc_publisher WHERE rsc_id=$1",
	SQL2 = "SELECT name FROM publishers WHERE id=$1",
	Names = many_to_many(SQL1, SQL2, ID),
	enumerate_bin_list(Names).

get_files(ID) ->
	SQL1 = "SELECT fileinfo_id FROM rsc_fileinfo WHERE rsc_id=$1",
	SQL2 = "SELECT name FROM fileinfos WHERE id=$1",
	Names = many_to_many(SQL1, SQL2, ID),
	enumerate_bin_list(Names).

get_disciplines(ID) ->
	SQL1 = "SELECT discipline_id FROM rsc_discipline WHERE rsc_id=$1",
	SQL2 = "SELECT name FROM disciplines WHERE id=$1",
	Names = many_to_many(SQL1, SQL2, ID),
	enumerate_bin_list(Names).
	
add_authors_to_rsc(ID, AuthorIDs) when is_list(AuthorIDs) ->
	SQL = "INSERT INTO rsc_author VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, AuthorIDs).

add_publishers_to_rsc(ID, PublisherIDs) when is_list(PublisherIDs) ->
	SQL = "INSERT INTO rsc_publisher VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, PublisherIDs).

add_files_to_rsc(ID, FileIDs) when is_list(FileIDs) ->
	SQL = "INSERT INTO rsc_fileinfo VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, FileIDs).

add_disciplines_to_rsc(ID, DisciplineIDs) when is_list(DisciplineIDs) ->
	SQL = "INSERT INTO rsc_discipline VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, DisciplineIDs).

many_to_many(SQL1, SQL2, ID) ->
	case sql:equery(SQL1,[ID]) of
		{ok, _, IDs} ->
			lists:filter(fun(Bin)->Bin =/= undefined end,
				lists:map( fun({Id}) ->
					case sql:equery(SQL2,[Id]) of 
						{ok, _, [{Name}]} -> Name;
						_ -> undefined
					end
				end, IDs));
		_ -> []
	end.