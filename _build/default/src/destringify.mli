open Dbtype

exception InvalidDB

val read_file : string -> db
(**Parses a json file into a database*)
