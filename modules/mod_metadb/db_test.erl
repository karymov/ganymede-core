-module(db_test).
-compile(export_all).
-include("ganymede.hrl").

category() ->
    {#rsc_meta{
        name="test cat",
        type=node_type(category)
    }, [{disciplines,[0,1]}] }.
    
book() ->
    {#rsc_meta{
        name="test book",
        type=node_type(book),
        description="just another test book",
        year=2010,
        url="freetestbooks.org",
        pages_count=1234
        % format="PDF"
    }, [{authors,[0,1]},{disciplines,[0,1]},{files,[0]},{publishers,[0]}] }.
    
video() ->
    {#rsc_meta{
        name="Pirates of Silicon Valley",
        type=node_type(video),
        description="this film is about apple and microsoft",
        year=1996,
        url="rutracker.org",
        duration=7200
        % format="AVI, DIVX 2Mb/sec, MP3, Stereo, 320kb/s"
    }, [{authors,[0,1]},{disciplines,[0,1]},{files,[0]},{publishers,[0]}] }.

audio() ->
    {#rsc_meta{
        name="Harder, Better, Faster, Stronger",
        type=node_type(audio),
        description="Daft Punk",
        year=2000,
        url="rutracker.org",
        duration=300
        % format="ACC, Stereo, 192kb/s"
    }, [{files,[0]}] }.

present() ->
    {#rsc_meta{
        name="Кое-что про Erlang",
        type=node_type(presentation),
        description="Кое-что про Erlang, а также OCaml, Haskell Perl, PHP, C и C++",
        year=2010,
        url="lionet.info",
        duration=300,
        pages_count=1234
        % format="PDF"
    }, [{files,[0]}] }.

add(ParentID, X, Y, 0) -> ok;
add(ParentID, X, Y, Depth) ->
    lists:foreach( fun(N) ->
            {B, Brd}=book(), meta_db:add_rsc(ParentID, B, Brd),
            {V, Vrd}=video(), meta_db:add_rsc(ParentID, V, Vrd),
            {A, Ard}=audio(), meta_db:add_rsc(ParentID, A, Ard),
            {P, Prd}=present(), meta_db:add_rsc(ParentID, P, Prd)
        end,
        lists:seq(1,Y)),
    lists:foreach( fun(N) ->
            {C, Crd}=category(), meta_db:add_rsc(ParentID, C, Crd)
        end,
        lists:seq(1,X)),
    lists:foreach(fun({ID,Name,Type}) ->
        case Type of
            0 -> add(ID, X, Y, Depth-1);
            _ -> ok
        end
    end,  meta_tree:children(ParentID)).

run(X, Y, D) ->
    
    sql:equery("DELETE FROM nodes",[]),
    sql:equery("DELETE FROM rsc_metas",[]),
    sql:equery("DELETE FROM rsc_author",[]),
    sql:equery("DELETE FROM rsc_discipline",[]),
    sql:equery("DELETE FROM rsc_fileinfo",[]),
    sql:equery("DELETE FROM rsc_publisher",[]),
    
    {ok,RootID} = meta_tree:make_root(),
    
    io:format("\t::test start..~n"),
    {A1, B1, C1} = now(),
    T1 = A1 * 1000000000 + B1 * 1000000 + C1,
        
    add(RootID, X, Y, D),
    
    {A2, B2, C2} = now(),
    T2 = A2 * 1000000000 + B2 * 1000000 + C2,
    io:format("\t::test end.~n"),
    io:format("\t -time: ~pms~n",[(T2-T1) div 1000]),
    io:format("\t -nodes: ~p~n",[ (math:pow(X, D)-1) / (X-1) * (X+Y*4) ]).

n(X, Y, D) ->
    (math:pow(X, D)-1) / (X-1) * (X+Y*4).