open Dbtype

type csv = string list list

val to_csv : table -> csv
val save_csv : table -> unit
