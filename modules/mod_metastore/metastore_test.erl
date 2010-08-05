-module(metastore_test).
% -compile(export_all).
% -include("ganymede.hrl").
% 
% book() ->
%     #book_meta{name = "Test Book", year = 1254, pages_count = 345, abstract = "books about cs", discipline = "computer science"}.
% category() ->
%     #category_meta{name = "Test Category", discipline = "informatics", description = "books about informatics"}.
% 
% go(X, Y, Z) ->
%     g_db:start_link(default),
%     g_db:delete("DELETE FROM data_nodes"),
%     g_db:delete("DELETE FROM book_metas"),
%     g_db:delete("DELETE FROM category_metas"),
%     metastore_db:make_root(),
%     io:format("\t::test start..~n"),
%     {A1, B1, C1} = now(),
%     T1 = A1 * 1000000000 + B1 * 1000000 + C1,
%     
%     
%     lists:foreach( fun(N) ->
%         Name = "level1_cat_" ++ integer_to_list(N),
%         metastore:put(#data_node{ id = Name, name = Name, type = category}, category(), "root")
%         end,
%         lists:seq(1,X)),
%     
%     Root = metastore:get_node("root"),
%     
%     lists:foreach(fun(Name) ->
%         lists:foreach(fun(N) ->
%             BName = "book_" ++ integer_to_list(N),
%             metastore:put(#data_node{ id = BName, name = BName, type = book}, book(), Name)
%             end,
%             lists:seq(1,Y))
%         end,
%         Root#data_node.children),
%         
%     lists:foreach(fun(Name) ->    
%         lists:foreach(fun(N) ->
%             BName = "level2_cat_" ++ integer_to_list(N),
%             metastore:put(#data_node{ id = BName, name = BName, type = category}, category(), Name)
%             end,
%             lists:seq(1,Z))
%         end,
%         Root#data_node.children),
%     
%     {A2, B2, C2} = now(),
%     T2 = A2 * 1000000000 + B2 * 1000000 + C2,
%     io:format("\t::test end.~n"),
%     io:format("~nTime: ~pms~n",[(T2-T1) div 1000]).