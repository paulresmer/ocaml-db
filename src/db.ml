open Dbtype
open Stringify

exception InvalidAdd
exception InvalidTableName

let add_to_col (value : value) (column : column) =
  let new_vals =
    match (value, column.col_type) with
    | VInt i, TInt -> VInt i :: column.values
    | VFloat f, TFloat -> VFloat f :: column.values
    | VString s, TString -> VString s :: column.values
    | VBool b, TBool -> VBool b :: column.values
    | _ -> raise InvalidAdd
  in
  { column with values = new_vals }

let add_to_tbl (column : column) (table : table) =
  { table with cols = column :: table.cols }

let rec insert_helper (values : value list) (columns : column list)
    (table : table) =
  match (values, columns) with
  | [], [] -> table
  | v_head :: v_tail, c_head :: c_tail ->
      let new_col = add_to_col v_head c_head in
      insert_helper v_tail c_tail (add_to_tbl new_col table)
  | [], _ -> raise InvalidAdd
  | _, [] -> raise InvalidAdd

let insert_row (row : value list) (table : table) =
  insert_helper row table.cols table

let get_col (name : string) (tbl : table) =
  List.find (fun col -> col.name = name) tbl.cols

let rename_col (name : string) (column : column) = { column with name }
let col_name (col : column) = col.name

let init_table (name : string) (db : db) =
  let new_table = { title = name; cols = [] } in
  if List.mem name (List.map (fun table -> table.title) db) then
    raise InvalidAdd
  else
    let newdb = new_table :: db in
    save newdb "db.json";
    newdb

let cols_of_table (name : string) (db : db) =
  if not (List.mem name (List.map (fun table -> table.title) db)) then
    raise InvalidTableName
  else
    let table = List.find (fun elt -> elt.title = name) db in
    table.cols

let table_title (tbl : table) = tbl.title

let drop_tbl (title : string) (db : db) =
  if not (List.mem title (List.map (fun table -> table.title) db)) then
    raise InvalidTableName
  else
    let newdb = List.filter (fun table -> table.title <> title) db in
    save newdb "db.json";
    newdb

let count_tbl (title : string) (db : db) =
  if not (List.mem title (List.map (fun table -> table.title) db)) then
    raise InvalidTableName
  else
    let table = List.find (fun table -> table.title = title) db in
    List.length table.cols
