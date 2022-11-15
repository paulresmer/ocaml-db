(* open Db *)
open Dbtype

val current_database : db ref
val current_file : string ref
val create_table : string -> unit
val describe_cols : string -> unit
val load_db : string -> unit
val describe_tbls : unit -> unit
val drop_tbl : string -> unit
val count_tbl : string -> unit
val insert_into : string list -> unit
val add_col : string list -> unit
val help : unit -> unit
