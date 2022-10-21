open Dbtype

exception InvalidAdd
exception InvalidTableName

(*Adds value to a column. Returns new column*)
val add_to_col : value -> column -> column

(*Removes value from a column. Returns new column. Does not do anything if value
  is not present in column*)
val rem_from_col : value -> column -> column

(*Renames column*)
val rename_col : string -> column -> column

val col_name : column -> string
(** Gets column name*)

(*Creates a new, empty table, returns the table*)
val init_table : string -> db -> db

(*List of columns in table*)
val cols_of_table : string -> db -> column list

val title_of_table : table -> string
(**Title of table*)

val drop_tbl : string -> db -> db
(**New database with table with title [tbl] dropped*)

val count_tbl : string -> db -> int
(**Number of rows in table [tbl]*)

(**String repr of rows in table [tbl]*)
(* val select_all: string -> db -> string *)