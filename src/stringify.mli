open Dbtype

(*[stringify v] is the string representation of value [v]*)
val stringify : value -> string

(*[stringify c] is the string representation of column [c]*)
val stringify_col : column -> string

(*[stringify tbl] is the string representation of table [tbl]*)
val stringify_table : table -> string

(*[stringify db] is the string representation of database [db]*)
val stringify_db : db -> string

(*[save db file] saves the database [db] in file [file]*)
val save : db -> string -> unit
