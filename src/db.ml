open Dbtype
open Stringify

exception InvalidAdd
exception InvalidTableName
exception InvalidInsert
exception InvalidColType
exception ColumnValueMismatch
exception InvalidColumn
exception PrimaryColumnAlreadyExists
exception InvalidNumericColumn
exception PrimaryKeyAlreadyExists
exception InvalidFind

let add_to_col (value : value) (column : column) =
  let new_vals =
    match (value, column.col_type) with
    | VInt i, TInt -> VInt i :: column.values
    | VFloat f, TFloat -> VFloat f :: column.values
    | VString s, TString -> VString s :: column.values
    | VBool b, TBool -> VBool b :: column.values
    | VInt i, TPrim -> (
        let int_lst =
          List.map
            (fun elt ->
              match elt with
              | VPrim i -> i
              | VNull -> 0
              | _ -> raise InvalidAdd)
            column.values
        in
        match List.find_opt (fun elt -> elt = i) int_lst with
        | None -> VPrim i :: column.values
        | Some _ -> raise PrimaryKeyAlreadyExists)
    | _, _ -> raise InvalidAdd
  in
  { column with values = new_vals }

let add_to_tbl (column : column) (table : table) =
  let removed = List.filter (fun col -> col.name <> column.name) table.cols in
  { table with cols = column :: removed }

let rec primitive_to_values (primitives : string list) (columns : column list)
    (values : 'a list) =
  match (primitives, columns) with
  | [], [] -> List.rev values
  | p_head :: p_tail, c_head :: c_tail -> (
      match c_head.col_type with
      | TBool ->
          let v = from_bool (bool_of_string p_head) in
          primitive_to_values p_tail c_tail (v :: values)
      | TFloat ->
          let v = from_float (float_of_string p_head) in
          primitive_to_values p_tail c_tail (v :: values)
      | TString ->
          primitive_to_values p_tail c_tail (from_string p_head :: values)
      | TInt ->
          let v = from_int (int_of_string p_head) in
          primitive_to_values p_tail c_tail (v :: values)
      | TPrim ->
          let v = from_int (int_of_string p_head) in
          primitive_to_values p_tail c_tail (v :: values))
  | [], _ -> raise ColumnValueMismatch
  | _, [] -> raise InvalidInsert

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

let find_table (name : string) (db : db) =
  if not (List.mem name (List.map (fun table -> table.title) db)) then
    raise InvalidTableName
  else List.find (fun elt -> elt.title = name) db

let table_title (tbl : table) = tbl.title
let retitle_tbl (title : string) (table : table) = { table with title }

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

let update_tbl (table : table) (db : db) =
  let removed = List.filter (fun elt -> elt.title <> table.title) db in
  table :: removed

let check_if_primary_exists (table : table) =
  let rec iterate (cols : column list) =
    match cols with
    | [] -> false
    | h :: t -> if h.col_type = TPrim then true else iterate t
  in
  iterate table.cols

let init_col (name : string) (c_type : string) (table : table) =
  let col_type =
    match String.capitalize_ascii c_type with
    | "INT" -> TInt
    | "FLOAT" -> TFloat
    | "BOOL" -> TBool
    | "STRING" -> TString
    | "PRIM" ->
        if check_if_primary_exists table then raise PrimaryColumnAlreadyExists
        else TPrim
    | _ -> raise InvalidColType
  in

  let cols = table.cols in
  let rec find_max_size cols acc =
    match cols with
    | [] -> acc
    | h :: t ->
        if List.length h.values > acc then
          find_max_size t (List.length h.values)
        else find_max_size t acc
  in
  let max_size = find_max_size cols 0 in
  let values =
    if max_size <> 0 then List.init max_size (fun _ -> Dbtype.null) else []
  in
  add_to_tbl { name; values; col_type } table

let rename_col (name : string) (col : column) = { col with name }

let find_prim (key : int) (table : table) =
  if not (check_if_primary_exists table) then raise InvalidFind
  else
    let rec find_idx (cols : column list) =
      match cols with
      | [] -> raise InvalidFind
      | h :: t -> (
          match h.col_type with
          | TPrim ->
              let vals =
                List.map
                  (fun elt ->
                    match elt with
                    | VPrim i -> i
                    | _ -> raise InvalidFind)
                  h.values
              in
              let rec find_by_index (lst : int list) (key : int) (acc : int) =
                match lst with
                | [] -> -1
                | h :: t ->
                    if h = key then acc else find_by_index t key (acc + 1)
              in
              find_by_index vals key 0
          | _ -> find_idx t)
    in
    let index = find_idx table.cols in
    let rec find_all (cols : column list) (acc : string list) =
      match cols with
      | [] -> acc
      | h :: t ->
          let str =
            h.name ^ "(" ^ type_to_string h.col_type ^ "): "
            ^ to_string (List.nth h.values index)
          in
          find_all t (str :: acc)
    in
    find_all table.cols []
