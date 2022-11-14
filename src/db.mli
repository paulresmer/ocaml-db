open Dbtype

exception InvalidAdd
exception InvalidTableName

(*[add_to_col v c] is the column [c] with the value [v] added to it.*)
val add_to_col : value -> column -> column

(*[rem_from_col v c] is the column [c] with the value [v] removed from it.*)
val rem_from_col : value -> column -> column

(*[rename_col s c] is a copy of the column [c] with its name changed to [s].*)
val rename_col : string -> column -> column

(*[col_name c] is the name of the column c*)
val col_name : column -> string

(*[get_col name tbl] is the is column with name [name] in table [tbl]*)
val get_col : string -> table -> column

(*[init_table name db] is the database [db] with an additional empty table with
  name [name]*)
val init_table : string -> db -> db

(*[cols_of_table name db] is the list of columns in table [name] in db [db]*)
val cols_of_table : string -> db -> column list

(*[table_title tbl] is the title of table [tbl] *)
val table_title : table -> string

(*[drop_tbl tbl db] is the database [db] with table [tbl] removed*)
val drop_tbl : string -> db -> db

(*[count_tbl tbl db] is a count of the columns in table [tbl] in database
  [db] *)
val count_tbl : string -> db -> int
