open Dbtype

exception InvalidAdd
exception InvalidTableName

(*[insert_row v_l t] is the table [t] with a new row of values [v_l] appended to
  it.*)
val insert_row : value list -> table -> table

(*[col_name c] is the name of the column c*)
val col_name : column -> string

(*[get_col name tbl] is the column with name [name] in table [tbl]*)
val get_col : string -> table -> column

(*[rename_col name col] is column [col] with name [name]*)
val rename_col : string -> column -> column

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
