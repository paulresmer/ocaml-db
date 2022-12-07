open Dbtype

val save_to_cloud : db -> unit
(**[save_to_cloud db] stringifies the database [db] and saves it to a remote
   URL, using the JSONBINS API*)

val download : string -> db
(**[download id] looks for a database with id [id]. If it exists, it downloads
   the JSON and parses it to a db.*)
