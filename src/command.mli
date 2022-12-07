exception Empty
(**An exception of type [Empty] is raised when the user inputs some variation of
   the empty string for eg. the empty string surrounded by spaces. *)

exception Malformed
(**An exception of type [Malformed] is raised when the user inputs a command
   that is not empty but does not conform to the command rules. *)

type command =
  | Help  (**The [Help] command displays the help meny*)
  | TableInit of string
      (**The [TableInit name] command creates a new table in the current
         database with name [name]*)
  | DescribeCols of string
      (** The [DescribeCols name] command prints out the names of the columns in
          table [name]*)
  | DescribeTbls
      (** The [DescribeTbls] command prints out the names of the tables in the
          current database*)
  | LoadDB of string
      (** The [LoadDB name] command sets the current database as the database
          described in the file [name.json]*)
  | DropTbl of string
      (**The [DropTbl name] command drops the table [name] from the current
         database*)
  | CountTbl of string
      (**The [CountTbl name] command counts the number of tables in the current
         database*)
  | InsertRow of string list
      (**The [InsertRow lst] command parses the list [lst] and inserts the
         specified values into the specified table*)
  | AddCols of string list
      (**The [AddCols lst] command parses the list [lst] and adds the specified
         columns into the specified table*)
  | Quit  (**The [Quit] command exits the REPL.*)
  | PrintTbl of string  (**The [PrintTbl t] command pretty-prints table [t].*)
  | Push  (**The [Push] command pushes the database to a remote URL.*)
  | SaveCSV of string  (**The [SaveCSV t] exports table [t] as a csv file*)
  | Sum of string
      (**The [Sum t.c] command returns the sum of column [c] in table [t]*)
  | Mean of string
      (**The [Mean t.c] command returns the mean of column [c] in table [t]*)
  | Median of string
      (**The [Median t.c] command returns the median of column [c] in table [t]*)
  | Max of string
      (**The [Max t.c] command returns the max of column [c] in table [t]*)
  | Min of string
      (**The [Min t.c] command returns min max of column [c] in table [t]*)
  | Var of string
      (**The [Var t.c] command returns the variance of column [c] in table [t]*)
  | StdDev of string
      (**The [StdDev t.c] command returns the variance of column [c] in table
         [t]*)
  | Pull of string
      (**The [Pull id] command downloads the database with id [id] from the
         cloud*)
  | FindPrim of string list
      (**The [FindPrim lst] command finds the row with Primary Key specified in
         lst in table specified in lst*)
  | LoadCSV of string list
      (**The [LoadCSV f INTO t] command loads the contents of csv file [f] into
         table [t]*)
  | FindWhere of string list
      (**The [FINDWHERE t] command prints the rows that satisfy the predicate,
         where these parameters are specified in t*)

(**A value of type [command] represents a particular command that can be input
   by a user to interact with the database*)

val parse : string -> command
(**[parse str] is the command described by the string [str]. If [str] is empty
   (or some variation of empty), [Empty] is raised. If [str] does not represent
   a valid command, [Malformed] is raised*)
