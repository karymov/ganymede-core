-module(metastore_test).
-compile(export_all).
-include("ganymede.hrl").

book() ->
    #book_meta{name = "Test Book", year = 1254, pages_count = 345, abstract = "books about cs", discipline = "computer science"}.
category() ->
    #category_meta{name = "Test Category", discipline = "informatics", description = "books about informatics"}.

go(X, Y, Z) ->
    sql:equery("DELETE FROM data_nodes",[]),
    sql:equery("DELETE FROM book_metas",[]),
    sql:equery("DELETE FROM category_metas",[]),
    metastore:make_root(),
    
    io:format("\t::test start..~n"),
    {A1, B1, C1} = now(),
    T1 = A1 * 1000000000 + B1 * 1000000 + C1,
    
    
    lists:foreach( fun(N) ->
        Name = "level1_cat_" ++ integer_to_list(N),
        metastore:put(#data_node{name = Name, type = category}, category(), 0)
        end,
        lists:seq(1,X)),
    
    Root = metastore:get_root(),
    
    lists:foreach(fun({ID,Name,Type}) ->
        lists:foreach(fun(N) ->
            BName = "book_" ++ integer_to_list(N),
            metastore:put(#data_node{name = BName, type = book}, book(), ID)
            end,
            lists:seq(1,Y))
        end,
        metastore:get_children(Root#data_node.id)),
        
    lists:foreach(fun({ID,Name,Type}) ->    
        lists:foreach(fun(N) ->
            BName = "level2_cat_" ++ integer_to_list(N),
            metastore:put(#data_node{name = BName, type = category}, category(), ID)
            end,
            lists:seq(1,Z))
        end,
        metastore:get_children(Root#data_node.id)),
    
    {A2, B2, C2} = now(),
    T2 = A2 * 1000000000 + B2 * 1000000 + C2,
    io:format("\t::test end.~n"),
    io:format("\t -time: ~pms~n",[(T2-T1) div 1000]),
    io:format("\t -nodes: ~p~n",[X*(Y+Z)]).