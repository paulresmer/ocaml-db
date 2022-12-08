open Dbtype

exception InvalidDB
(**An exception of type [InvalidDB] is raised when an invalid .json file is
   passed in to the [LOAD] command*)

val read_file : string -> db
(**[read_file file] is the database of type [db] represented in the json file
   [file]. Raises [InvalidDB] if the file cannot be parsed as a database*)

val parse_string : string -> db
(**[parse_string json] is the database of type [db] represented in the string
   [json]. Raises [InvalidDB] if the file cannot be parsed as a database*)
