open Dbtype

(*Returns string repr of value*)
val stringify : value -> string

(*Returns string repr of column*)
val stringify_col : column -> string

(*Returns string repr of table*)
val stringify_table : table -> string

(*Returns string repr of entire db*)
val stringify_db : db -> string

(*Saves db*)
val save : db -> string -> unit
