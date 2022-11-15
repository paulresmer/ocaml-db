(*An exception of type [Empty] is raised when the user inputs some variation of
  the empty string for eg. the empty string surrounded by spaces. *)
exception Empty

(*An exception of type [Malformed] is raised when the user inputs a command that
  is not empty but does not conform to the command rules. *)
exception Malformed

(*A value of type [command] represents a particular command that can be input by
  a user to interact with the database*)
type command =
  (*The [Help] command displays the help menu *)
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

(*[parse str] is the command described by the string [str]. If [str] is empty
  (or some variation of empty), [Empty] is raised. If [str] does not represent a
  valid command, [Malformed] is raised*)
val parse : string -> command
