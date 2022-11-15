open Dbtype

(*[current_database] is a reference to the database in scope *)
val current_database : db ref

(*[current_file] is a reference to the file in scope i.e. the file the current
  database is abstracted from*)
val current_file : string ref

(*[create_table t] creates a new table [t]*)
val create_table : string -> unit

(*[describe_cols t] describes the columns of table [t]*)
val describe_cols : string -> unit

(*[load_db db] changes the current database to [db]*)
val load_db : string -> unit

(*[describe_tbls] describes the tables of the current database*)
val describe_tbls : unit -> unit

(*[drop_tbl t] is the current database with table [t] dropped*)
val drop_tbl : string -> unit

(*[count_tbl t] prints the number of rows in table [t]*)
val count_tbl : string -> unit

(*[insert_into vals] inserts [vals] into the table specified in [vals]*)
val insert_into : string list -> unit

(*[add_col vals] adds a column specified in [vals] into the table specified in
  [vals]*)
val add_col : string list -> unit

(*[help] prints the help menu*)
val help : unit -> unit

(*[quit] exits the REPL*)
val quit : unit -> unit
