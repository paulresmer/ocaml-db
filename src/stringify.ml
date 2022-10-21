open Dbtype

let stringify value = "\"" ^ to_string value ^ "\""

let stringify_col col =
  let vals_stringified =
    col.values |> List.map (fun elt -> stringify elt) |> String.concat ", "
  in
  "{ \"name\":\"" ^ col.name ^ "\",  \"values\": [" ^ vals_stringified
  ^ "],\"type\": \""
  ^ type_to_string col.col_type
  ^ "\"}"

let stringify_table tbl =
  let cols_list = List.map (fun elt -> stringify_col elt) tbl.cols in
  let cols_stringified = String.concat "," cols_list in
  "{ \"title\":\"" ^ tbl.title ^ "\",  \"columns\": [" ^ cols_stringified ^ "]}"

let stringify_db db =
  let tbls_list = List.map (fun elt -> stringify_table elt) db in
  let tbls_stringified = String.concat "," tbls_list in
  "{ \"tables\": [" ^ tbls_stringified ^ "]}"

let save db file =
  let overwrite = open_out file in
  Printf.fprintf overwrite "%s" (stringify_db db);
  close_out overwrite
