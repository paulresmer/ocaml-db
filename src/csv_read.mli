open Dbtype

exception MalformedCSV
(**Raised when a csv is invalid*)

exception HeterogeneousCols
(**Raised when the columns in a csv file are not discernibly homogenous*)

type csv = string list list
(**Represent a csv as a nested list*)

val load : string -> table
(**[load f] is the table representation of the data in f. Raises [MalformedCSV]
   if the .csv format is not respected or the file does not exist.*)
