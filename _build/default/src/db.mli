open Dbtype

exception InvalidAdd
exception InvalidTableName
exception InvalidInsert
exception InvalidColType
exception ColumnValueMismatch
exception InvalidColumn
exception InvalidNumericColumn
exception PrimaryColumnAlreadyExists
exception PrimaryKeyAlreadyExists
exception InvalidFind

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

(*[primitive_to_values s c] is the list of values resulting from converting each
  string element of [s] to a value, based on the types of the column [c]*)
val primitive_to_values : string list -> column list -> value list -> value list

(*[cols_of_table name db] is the list of columns in table [name] in db [db]*)
val cols_of_table : string -> db -> column list

(*[table_title tbl] is the title of table [tbl] *)
val table_title : table -> string

(*[retitle_tbl name tbl] is table [tbl] with title [title]*)
val retitle_tbl : string -> table -> table

(*[drop_tbl title db] is the database [db] with the table [tbl] with title
  [title] removed *)
val drop_tbl : string -> db -> db

(*[count_tbl title db] is a count of the columns in the table [tbl] with title
  [title] in database [db] *)
val count_tbl : string -> db -> int

(*[find_table tbl db] is the table with name [tbl] in database [db]*)
val find_table : string -> db -> table

(*[update_tbl tbl db] is the database [db] with table [tbl] updated*)
val update_tbl : table -> db -> db

(*[init_col name type table] is the table [table] with a new column with name
  [name] and column type [type]*)
val init_col : string -> string -> table -> table

(*[find_prim id table] returns the row with id [id] in table [table]*)
val find_prim : int -> table -> string list

(*[table_exists name db] is true if a table with title [name] exists in database
  [db], false otherwise*)
val table_exists : string -> db -> bool
