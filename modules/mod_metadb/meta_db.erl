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

-export([add_book/0, add_video/0, add_audio/0, add_presentation/0]).
-export([add_person/0, add_publisher/0, add_fileinfo/0, add_cover/0]).
-export([add_discipline/0, add_tag/0, monitor/0]).

-export([get_rsc/0]).

-export([init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-include("ganymede.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec add_book(CategoryID)
%% @end
add_book() ->
    ok.

%% @spec
%% @end
add_video() ->
    ok.

%% @spec
%% @end
add_audio() ->
    ok.

%% @spec
%% @end
add_presentation() ->
    ok.
    
%% @spec
%% @end
add_person() ->
    ok.

%% @spec
%% @end
add_publisher() ->
    ok.
    
%% @spec
%% @end
add_fileinfo() ->
    ok.
    
%% @spec
%% @end
add_cover() ->
    ok.
    
% @spec
% @end
add_discipline() ->
    ok.
    
%% @spec
%% @end
add_tag() ->
    ok.

%% @spec
%% @end
get_rsc() ->
    ok.

%% @spec
%% @end
monitor() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
    {ok, []}.

handle_call(_, _From, State) ->
    {reply, ok, State}.
    
handle_cast(_, State) -> {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.