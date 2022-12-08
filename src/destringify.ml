open Dbtype

exception InvalidDB

open Yojson.Basic.Util

let col_map_helper col =
  let col_type = col |> member "type" |> to_string in
  let values = col |> member "values" |> to_list in
  let name = col |> member "name" |> to_string in
  match col_type with
  | "Int" ->
      {
        col_type = TInt;
        values =
          List.map
            (fun elt ->
              elt |> to_string
              |> (fun elt -> if elt = "NULL" then 0 else int_of_string elt)
              |> from_int)
            values;
        name;
      }
  | "String" ->
      {
        col_type = TString;
        values = List.map (fun elt -> elt |> to_string |> from_string) values;
        name;
      }
  | "Float" ->
      {
        col_type = TFloat;
        values =
          List.map
            (fun elt ->
              elt |> to_string
              |> (fun elt -> if elt = "NULL" then 0. else float_of_string elt)
              |> from_float)
            values;
        name;
      }
  | "Bool" ->
      {
        col_type = TBool;
        values =
          List.map
            (fun elt ->
              elt |> to_string
              |> (fun elt -> if elt = "NULL" then false else bool_of_string elt)
              |> from_bool)
            values;
        name;
      }
  | "Primary" ->
      let clean =
        if Yojson.Basic.Util.to_string (List.hd values) = "NULL" then
          List.init (List.length values) (fun i -> VPrim (i + 1))
        else
          List.map
            (fun elt ->
              elt |> to_string
              |> Str.(global_replace (regexp "#") "")
              |> int_of_string
              |> fun elt -> VPrim elt)
            values
      in
      { col_type = TPrim; values = clean; name }
  | _ -> raise InvalidDB

let table_map_helper tbl =
  let cols = tbl |> member "columns" |> to_list in
  let title = tbl |> member "title" |> to_string in
  { cols = List.map col_map_helper cols; title }

let read_file (name : string) : db =
  let path = name ^ ".json" in
  if not (Sys.file_exists path) then raise InvalidDB
  else
    let tables = Yojson.Basic.from_file path |> member "tables" |> to_list in
    List.map table_map_helper tables

let parse_string (json : string) : db =
  let tables =
    Yojson.Basic.from_string json
    |> member "record" |> member "tables" |> to_list
  in
  List.map table_map_helper tables
