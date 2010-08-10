sql_table(account) -> " accounts ";
sql_table(data_node) -> " data_nodes ";
sql_table(book_meta) -> " book_metas ";
sql_table(category_meta) -> " category_metas ";
sql_table(person_meta) -> " person_metas ";
sql_table(publisher_meta) -> " publisher_metas ".

sql_insert_pattern(account) ->
    {"(login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"};
sql_insert_pattern(data_node) ->
    {"(name,type,parent)",
    "($1,$2,$3)"};
sql_insert_pattern(book_meta) ->
    {"(name,author_id,publisher_id,year,pages_count,description,discipline)",
    "($1,$2,$3,$4,$5,$6,$7)"};
sql_insert_pattern(category_meta) ->
    {"(name,discipline,description)",
    "($1,$2,$3)"};
sql_insert_pattern(person_meta) ->
    {"(name,surname)",
    "($1,$2)"};
sql_insert_pattern(publisher_meta) ->
    {"(name)",
    "($1)"}.

sql_insert2_pattern(account) ->
    {"(id,login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)"};
sql_insert2_pattern(data_node) ->
    {"(id,name,type,parent)",
    "($1,$2,$3,$4)"};
sql_insert2_pattern(book_meta) ->
    {"(id,name,author_id,publisher_id,year,pages_count,description,discipline)",
    "($1,$2,$3,$4,$5,$6,$7,$8)"};
sql_insert2_pattern(category_meta) ->
    {"(id,name,discipline,description)",
    "($1,$2,$3,$4)"};
sql_insert2_pattern(person_meta) ->
    {"(id,name,surname)",
    "($1,$2,$3)"};
sql_insert2_pattern(publisher_meta) ->
    {"(id,name)",
    "($1,$2)"}.



sql_update_pattern(account) ->
    "person_id=$2,password=$3,name=$4,surname=$5,role=$6,state=$7,reg_datetime=$8,login_datetime=$9,description=$10,email=$11";
sql_update_pattern(data_node) ->
    "name=$2,type=$3,children=$4";
sql_update_pattern(book_meta) ->
    "name=$2,author_id=$3,publisher_id=$4,year=$5,pages_count=$6,description=$7,discipline=$8,resources_ids=$9,covers_ids=$10";
sql_update_pattern(category_meta) ->
    "name=$2,discipline=$3,description=$4,resources_ids=$5,covers_ids=$6";
sql_update_pattern(person_meta) ->
    "name=$2,surname=$3";
sql_update_pattern(publisher_meta) ->
    "name=$2".