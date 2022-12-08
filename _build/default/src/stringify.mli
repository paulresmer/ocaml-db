open Dbtype

val stringify : value -> string
(**[stringify v] is the string representation of value [v]*)

val stringify_col : column -> string
(**[stringify c] is the string representation of column [c]*)

val stringify_table : table -> string
(**[stringify tbl] is the string representation of table [tbl]*)

val stringify_db : db -> string
(**[stringify db] is the string representation of database [db]*)

val save : db -> string -> unit
(**[save db file] saves the database [db] in file [file]*)
