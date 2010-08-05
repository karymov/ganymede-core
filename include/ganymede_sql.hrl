sql_table(account) -> " accounts ";
sql_table(data_node) -> " data_nodes ";
sql_table(book_meta) -> " book_metas ";
sql_table(category_meta) -> " category_metas ".

sql_insert_pattern(account) ->
    {"(login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"};
sql_insert_pattern(data_node) ->
    {"(name,type,children)",
    "($1,$2,$3,$4)"};
sql_insert_pattern(book_meta) ->
    {"(name,author_id,publisher_id,year,pages_count,abstract,discipline,resources_ids,covers_ids)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9)"};
sql_insert_pattern(category_meta) ->
    {"(name,discipline,description,resources_ids,covers_ids)",
    "($1,$2,$3,$4,$5,$6)"}.

sql_update_pattern(account) ->
    "person_id=$2,password=$3,name=$4,surname=$5,role=$6,state=$7,reg_datetime=$8,login_datetime=$9,description=$10,email=$11";
sql_update_pattern(data_node) ->
    "name=$2,type=$3,children=$4";
sql_update_pattern(book_meta) ->
    "name=$2,author_id=$3,publisher_id=$4,year=$5,pages_count=$6,abstract=$7,discipline=$8,resources_ids=$9,covers_ids=$10";
sql_update_pattern(category_meta) ->
    "name=$2,discipline=$3,description=$4,resources_ids=$5,covers_ids=$6".