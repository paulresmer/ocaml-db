(*An exception of type [Empty] is raised when the user inputs some variation of
  the empty string for eg. the empty string surrounded by spaces. *)
exception Empty

(*An exception of type [Malformed] is raised when the user inputs a command that
  is not empty but does not conform to the command rules. *)
exception Malformed

(*A value of type [command] represents a particular command that can be input by
  a user to interact with the database*)
type command =
  (*The [Help] command displays the help meny*)
  | Help
  (*The [TableInit name] command creates a new table in the current database
    with name [name]*)
  | TableInit of string
  (*The [DescribeCols name] command prints out the names of the columns in table
    [name]*)
  | DescribeCols of string
  (*The [DescribeTbls] command prints out the names of the tables in the current
    database*)
  | DescribeTbls
  (*The [LoadDB name] command sets the current database as the database
    described in the file [name.json]*)
  | LoadDB of string
  (*The [DropTbl name] command drops the table [name] from the current
    database*)
  | DropTbl of string
  (*The [CountTbl name] command counts the number of tables in the current
    database*)
  | CountTbl of string
  (*The [InsertRow lst] command parses the list [lst] and inserts the specified
    values into the specified table*)
  | InsertRow of string list
    (*The [InsertRow lst] command parses the list [lst] and adds the specified
      columns into the specified table*)
  | AddCols of string list
  (*The [Quit] command exits the REPL.*)
  | Quit
  (*The [PrintTbl t] command pretty-prints table [t].*)
  | PrintTbl of string
  (*The [Push] command pushes the database to a remote URL.*)
  | Push
  (*The [SaveCSV t] exports table [t] as a csv file*)
  | SaveCSV of string
  (*The [Sum t.c] command returns the sum of column [c] in table [t]*)
  | Sum of string
  (*The [Mean t.c] command returns the mean of column [c] in table [t]*)
  | Mean of string
  (*The [Median t.c] command returns the median of column [c] in table [t]*)
  | Median of string
  (*The [Max t.c] command returns the max of column [c] in table [t]*)
  | Max of string
  (*The [Min t.c] command returns min max of column [c] in table [t]*)
  | Min of string
  (*The [Var t.c] command returns the variance of column [c] in table [t]*)
  | Var of string
  (*The [Pull id] command downloads the database with id [id] from the cloud*)
  | Pull of string
  (*The [FindPrim lst] command finds the row with Primary Key specified in lst
    in table specified in lst*)
  | FindPrim of string list

(*[parse str] is the command described by the string [str]. If [str] is empty
  (or some variation of empty), [Empty] is raised. If [str] does not represent a
  valid command, [Malformed] is raised*)
val parse : string -> command
