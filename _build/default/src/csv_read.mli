open Dbtype

exception MalformedCSV
exception HeterogeneousCols

(*Represent a csv as a nested list*)
type csv = string list list

(*[load f] is the table representation of the data in f. Raises [MalformedCSV]
  if the .csv format is not respected or the file does not exist.*)
val load : string -> table
