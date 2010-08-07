-module(metastore_test).
-compile(export_all).
-include("ganymede.hrl").

book() ->
    #book_meta{
        name = "SICP: Structure and Interpretation of Computer Programs",
        year = 1987,
        pages_count = 657,
        author_id = 0,
        publisher_id = 0,
        abstract = "Эта книга посвящается, с уважением и любовью, духу, который живет внутри компьютера.
        “Мне кажется, чрезвычайно важно, чтобы мы, занимаясь информатикой, по- лучали радость от общения с компьютером. С самого начала это было гро- мадным удовольствием. Конечно, время от времени встревали заказчики, и через какое-то время мы стали серьезно относиться к их жалобам. Нам стало казаться, что мы вправду отвечаем за то, чтобы эти машины использовались успешно и безошибочно. Я не думаю, что это так. Я считаю, что мы отвечаем за то, чтобы их тренировать, указывать им новые направления и поддержи- вать уют в доме. Я надеюсь, что информатика никогда не перестанет быть радостью. Я надеюсь, что мы не превратимся в миссионеров. Не надо чув- ствовать себя продавцом Библий. Таких в мире и так достаточно. То, что Вы знаете о программировании, могут выучить и другие. Не думайте, что в ва- ших руках ключ к успешной работе с компьютерами. Что у Вас, как я думаю и надеюсь, есть — это разум: способность увидеть в машине больше, чем Вы видели, когда Вас впервые к ней подвели, увидеть, что Вы способны сделать ее бo ́льшим.”
        Алан Дж. Перлис (1 апреля 1922 – 7 февраля 1990)",
        discipline = "Computer Science"
    }.
category() ->
    #category_meta{
        name = "Computer Science books",
        discipline = "computer science",
        description = "books about computer science"
    }.

add(ParentID, X, Y, 0) -> ok;
add(ParentID, X, Y, Depth) ->
    lists:foreach( fun(N) ->
        metastore:put(#data_node{type = book}, book(), ParentID)
        end,
        lists:seq(1,Y)),
    lists:foreach( fun(N) ->
        Name = "depth_"++integer_to_list(Depth)++"_cat_" ++ integer_to_list(N),
        metastore:put(#data_node{name = Name, type = category}, category(), ParentID)
        end,
        lists:seq(1,X)),
    lists:foreach(fun({ID,Name,Type}) ->
        case Type of
            0 -> add(ID, X, Y, Depth-1);
            _ -> ok
        end
    end,  metastore:get_children(ParentID)).

go(X, Y, D) ->
    sql:equery("DELETE FROM data_nodes",[]),
    sql:equery("DELETE FROM book_metas",[]),
    sql:equery("DELETE FROM category_metas",[]),
    metastore:make_root(),
    
    io:format("\t::test start..~n"),
    {A1, B1, C1} = now(),
    T1 = A1 * 1000000000 + B1 * 1000000 + C1,
        
    add(0, X, Y, D),
    
    {A2, B2, C2} = now(),
    T2 = A2 * 1000000000 + B2 * 1000000 + C2,
    io:format("\t::test end.~n"),
    io:format("\t -time: ~pms~n",[(T2-T1) div 1000]),
    io:format("\t -nodes: ~p~n",[ (math:pow(X, D)-1) / (X-1) * (X+Y) ]).
go_n(X, Y, D) ->
    (math:pow(X, D)-1) / (X-1) * (X+Y).