open Dbtype

val print_table : table -> unit
(**[print_table t] prints the table [t]*)

val print_col : column -> int -> unit
(**[print_col c] prints the column [c]*)

val max_width : column list -> int -> int
(**[max_width t] is the maximum width in terms of characters of table [t]*)
