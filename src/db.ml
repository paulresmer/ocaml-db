open Dbtype

exception InvalidAdd
exception InvalidTableName

open Stringify
(* open Yojson.Basic.Util *)

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

let rem_from_col (value : value) (column : column) =
  { column with values = List.filter (fun elt -> elt <> value) column.values }

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

let title_of_table (tbl : table) = tbl.title

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

(* let rec pp_vals = function | [] -> () | h::t -> print_endline
   (Dbtype.to_string h); pp_vals t

   (* *) let rec pp_col (col:column)= let _ = print_endline (col.name ^ ":" ^
   (type_to_string col.col_type)) in pp_vals col.values *)

(* let select_all (title:string) (db:db) = if not(List.mem title (List.map (fun
   table->table.title) db)) then raise InvalidTableName else let table =
   List.find (fun table->table.title = title) db in let cols = table |>
   stringify_table |> Yojson.Basic.from_string |> member "columns" |>
   destringify in

   List.map pp_col cols *)
