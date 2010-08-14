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

-export([add_rsc/3]).
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

%% @spec add_rsc(ParentID, RSC, RedurantDataPList) -> {ok, ID} | {error, Reason}
%%  ParentID = integer
%%  RSC = #rsc_meta{}
%% @end
add_rsc(ParentID, RSC, RedurantDataPList) when RSC#rsc_meta.type =/= null ->
    gen_server:call(?MODULE, {add_rsc, ParentID, RSC, RedurantDataPList}).

    
add_person() ->
    ok.
get_person(ID) ->
    ok.
add_publisher() ->
    ok.
get_publiser(ID) ->
    ok.
add_fileinfo() ->
    ok.
get_fileinfo() ->
    ok.
add_cover() ->
    ok.
get_cover() ->
    ok.
add_discipline() ->
    ok.  
get_discipline() ->
    ok.
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

handle_call({add_rsc, ParentID, Item, RedurantDataPList}, _From, State) ->
	R = case store_common_data(ParentID, Item) of
		{ok, ID} ->
			make_redundant_data_refs(ID, RedurantDataPList),
            commit_redundant_data(ID);
		{error, Reason} -> {error, Reason}
	end, {reply, R, State}.	
    
handle_cast(_, State) -> {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_common_data(ParentID, Item) -> meta_tree:add(Item, ParentID).

make_redundant_data_refs(ID, RedundantDataPropList) ->
    lists:foreach(
        fun({RedurantDataTag, IDs}) ->
            add_redurant_ref(RedurantDataTag, ID, IDs)
        end,
    RedundantDataPropList).

commit_redundant_data(ID) ->
	Meta = meta_tree:get(ID),
	RSC = Meta#rsc_meta{
		author=get_authors(ID),
		discipline=get_disciplines(ID),
		publisher=get_publishers(ID),
		file=get_files(ID)},
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
	
add_redurant_ref(authors, ID, AuthorIDs) when is_list(AuthorIDs) ->
	SQL = "INSERT INTO rsc_author VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, AuthorIDs);
add_redurant_ref(publishers, ID, PublisherIDs) when is_list(PublisherIDs) ->
	SQL = "INSERT INTO rsc_publisher VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, PublisherIDs);
add_redurant_ref(files, ID, FileIDs) when is_list(FileIDs) ->
	SQL = "INSERT INTO rsc_fileinfo VALUES ($1,$2)",
	lists:foreach( fun(Id) -> sql:equery(SQL, [ID, Id]) end, FileIDs);
add_redurant_ref(disciplines, ID, DisciplineIDs) when is_list(DisciplineIDs) ->
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