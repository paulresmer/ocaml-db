open Dbtype

type csv = string list list
(**Represent csv data as a list of string list where each string list is a row
   in the sheet*)

val to_csv : table -> csv
(**[to_csv t] is a value of type [csv] that represents the table [table] *)

val save_csv : table -> unit
(**[save_csv t] saves the table [t] as a .csv file in the parent directory
   containing this project *)
