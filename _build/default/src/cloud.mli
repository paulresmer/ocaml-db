open Dbtype

(*[save_to_cloud db] stringifies the database [db] and saves it to a remote URL,
  using the JSONBINS API*)
val save_to_cloud : db -> unit

(*[download id] looks for a database with id [id]. If it exists, it downloads
  the JSON and parses it to a db.*)
val download : string -> db
