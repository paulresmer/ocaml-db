open Dbtype

exception InvalidAdd
(**Raised when an invalid add operation is attempted.*)

exception InvalidTableName
(**Raised when an invalid table name is referenced.*)

exception InvalidInsert
(**Raised when an invalid insert is attempted.*)

exception InvalidColType
(**Raised when the type of a column is invalid.*)

exception ColumnValueMismatch
(**Raised when an invalid insert due to a mismatch between values passed in and
   values expected is attempted.*)

exception InvalidColumn
(**Raised when an invalid column name is referenced.*)

exception InvalidNumericColumn
(**Raised when a numeric operation is attempted on a non-numeric column.*)

exception PrimaryColumnAlreadyExists
(**Raised when a primary column already exists.*)

exception PrimaryKeyAlreadyExists
(**Raised when a primary key already exists.*)

exception InvalidFind
(**Raised when an invalid find operation is attempted.*)

val insert_row : value list -> table -> table
(**[insert_row v_l t] is the table [t] with a new row of values [v_l] appended
   to it.*)

val col_name : column -> string
(**[col_name c] is the name of the column c*)

val get_col : string -> table -> column
(**[get_col name tbl] is the column with name [name] in table [tbl]*)

val rename_col : string -> column -> column
(**[rename_col name col] is column [col] with name [name]*)

val init_table : string -> db -> db
(**[init_table name db] is the database [db] with an additional empty table with
   name [name]*)

val primitive_to_values : string list -> column list -> value list -> value list
(**[primitive_to_values s c] is the list of values resulting from converting
   each string element of [s] to a value, based on the types of the column [c]*)

val cols_of_table : string -> db -> column list
(**[cols_of_table name db] is the list of columns in table [name] in db [db]*)

val table_title : table -> string
(**[table_title tbl] is the title of table [tbl] *)

val retitle_tbl : string -> table -> table
(**[retitle_tbl name tbl] is table [tbl] with title [title]*)

val drop_tbl : string -> db -> db
(**[drop_tbl title db] is the database [db] with the table [tbl] with title
   [title] removed *)

val count_tbl : string -> db -> int
(**[count_tbl title db] is a count of the columns in the table [tbl] with title
   [title] in database [db] *)

val find_table : string -> db -> table
(**[find_table tbl db] is the table with name [tbl] in database [db]*)

val update_tbl : table -> db -> db
(**[update_tbl tbl db] is the database [db] with table [tbl] updated*)

val init_col : string -> string -> table -> table
(**[init_col name type table] is the table [table] with a new column with name
   [name] and column type [type]*)

val find_prim : int -> table -> string list
(**[find_prim id table] returns the row with id [id] in table [table]*)

val table_exists : string -> db -> bool
(**[table_exists name db] is true if a table with title [name] exists in
   database [db], false otherwise*)
