open Dbtype

exception TableExists
(**Raised when a table already exists *)

val current_database : db ref
(**[current_database] is a reference to the database in scope *)

val current_file : string ref
(**[current_file] is a reference to the file in scope i.e. the file the current
   database is abstracted from*)

val create_table : string -> unit
(**[create_table t] creates a new table [t]*)

val describe_cols : string -> unit
(**[describe_cols t] describes the columns of table [t]*)

val load_db : string -> unit
(**[load_db db] changes the current database to [db]*)

val describe_tbls : unit -> unit
(**[describe_tbls] describes the tables of the current database*)

val drop_tbl : string -> unit
(**[drop_tbl t] is the current database with table [t] dropped*)

val count_tbl : string -> unit
(**[count_tbl t] prints the number of rows in table [t]*)

val insert_into : string list -> unit
(**[insert_into vals] inserts [vals] into the table specified in [vals]*)

val add_col : string list -> unit
(**[add_col vals] adds a column specified in [vals] into the table specified in
   [vals]*)

val help : unit -> unit
(**[help] prints the help menu*)

val quit : unit -> unit
(**[quit] exits the REPL*)

val print_table : string -> unit
(**[print_table t] pretty-prints table [t]*)

val push : unit -> unit
(**[push] pushes the current database to a remote URL*)

val save_csv : string -> unit
(**[save_csv t] saves table [t] as a csv*)

val sum : string -> unit
(**[sum t.c] prints the sum of column c in table t*)

val mean : string -> unit
(**[mean t.c] prints the mean of column c in table t*)

val max : string -> unit
(**[max t.c] prints the max of column c in table t*)

val min : string -> unit
(**[min t.c] prints the min of column c in table t*)

val pull : string -> unit
(**[pull id] sets the current database to be the database with id [id] on the
   cloud*)

val find_all : string list -> unit
(**[find_all vals] finds the rows with id spec in [vals] in table spec in [vals]*)

val find_median : string -> unit
(**[find_median t.c] prints the median of column c in table t*)

val find_variance : string -> unit
(**[find_variance t.c] prints the variance of column c in table t*)

val find_dev : string -> unit
(**[find_dev t.c] prints the variance of column c in table t*)

val load_csv : string list -> unit
(**[load_csv vals] loads a csv into a table*)

val find_where : string list -> unit
(**[find_where t] runs the find-where command on the parameters specified in t*)
