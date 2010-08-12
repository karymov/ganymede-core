sql_table(account) -> " accounts ";
sql_table(node) -> " nodes ";
sql_table(rsc_meta) -> " rsc_metas ";
sql_table(category_meta) -> " category_metas ";
sql_table(person) -> " persons ";
sql_table(publisher) -> " publishers ";
sql_table(fileinfo) -> " fileinfos ";
sql_table(cover) -> " covers ";
sql_table(tag) -> " tags ".

sql_insert_pattern(account) ->
    {"(login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"};
sql_insert_pattern(node) ->
    {"(name,type,parent)",
    "($1,$2,$3)"};
sql_insert_pattern(rsc_meta) ->
    {"(name,type,author,description,year,discipline,url,cover,pages_count,size,publisher,duration)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)"};
sql_insert_pattern(category_meta) ->
    {"(name,discipline,description)",
    "($1,$2,$3)"}.

sql_insert2_pattern(account) ->
    {"(id,login,person_id,password,name,surname,email,description,role,state,reg_datetime,login_datetime)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)"};
sql_insert2_pattern(node) ->
    {"(id,name,type,parent)",
    "($1,$2,$3,$4)"};
sql_insert2_pattern(rsc_meta) ->
    {"(id,name,type,author,description,year,discipline,url,cover,pages_count,size,publisher,duration)",
    "($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)"};
sql_insert2_pattern(category_meta) ->
    {"(id,name,discipline,description)",
    "($1,$2,$3,$4)"}.


sql_update_pattern(account) ->
    "person_id=$2,password=$3,name=$4,surname=$5,role=$6,state=$7,reg_datetime=$8,login_datetime=$9,description=$10,email=$11";
sql_update_pattern(node) ->
    "name=$2,type=$3,children=$4";
sql_update_pattern(rsc_meta) ->
    "name=$2,type=$3,author=$4,description=$5,year=$6,discipline=$7,url=$8,cover=$9,pages_count=$10,size=$11,publisher=$12,duration=$13";
sql_update_pattern(category_meta) ->
    "name=$2,discipline=$3,description=$4,resources_ids=$5,covers_ids=$6".