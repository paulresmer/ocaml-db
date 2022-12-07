open Command
open Destringify
open Db
open Dbtype
open Stringify
open Printer
open Cloud
open Stats
open Csv_write
open Csv_read

exception TableExists

let current_file = ref "db"
let current_database = ref (read_file !current_file)

let print_function msg color =
  let _ = msg |> ANSITerminal.print_string color in
  ()

let help () =
  print_function
    "Available commands\n\n\
     Database CRUD: \n\
     >> CREATE t: create a new,empty table with name t.\n\
     >> COLS t: Print the columns of table t\n\
     >> LOAD n: Load file n.json as the current database.\n\
     >> TBLS: Display current tables.\n\
     >> DROP t: Drop table t from the db.\n\
     >> COUNT t: Display number of rows in table t.\n\
     >> ADD col TYPE to t: Add a new column col of type T YPE to table t\n\
     >> INSERT x1;...;xn INTO t: Add a new row to table t\n\
     >> LOADCSV t.csv: Load a csv file t into a table t_csv\n\n\
     Cloud Version Control: \n\
     >> PUSH: Push current database to a remote URL as JSON\n\
     >> PULL id: Set current database as the database with aa given remote id\n\n\
     Visualize: \n\
     >> SAVECSV t: Export table [t] as a .csv file\n\
     >> PRINT t: Print table t\n\
     >> PRINT t.c: Print column c in table t\n\n\
     Statistics: \n\
     >> MEAN t.c: Print the average of column c in table c, if column is \
     numeric.\n\
     >> MEDIAN t.c: Print the median of column c in table c, if column is \
     numeric.\n\
     >> SUM t.c: Print the sum of column c in table c, if column is numeric.\n\
     >> MAX t.c: Print the max value of column c in table c, if column is \
     numeric.\n\
     >> MIN t.c: Print the min of column c in table c, if column is numeric.\n\n\
     Query: \n\
     >> FINDPRIM n IN t: Print row in t with primary key n\n\
     >> FINDWHERE col (=/>/</<=/>=/!=) val IN t: Print the first row in table \
     t that satisfy the predicate\n"
    [ ANSITerminal.cyan ]

let create_table (name : string) =
  let newdb = init_table name !current_database in
  let _ = current_database := newdb in
  print_function ("\nCreated table " ^ name) [ ANSITerminal.cyan ]

let describe_cols (name : string) =
  let cols = cols_of_table name !current_database in
  let col_names = List.map (fun elt -> col_name elt) cols in
  let col_types = List.map (fun elt -> type_to_string elt.col_type) cols in
  let col_annotations =
    List.map2
      (fun name col_type -> name ^ ":" ^ String.uppercase_ascii col_type)
      col_names col_types
  in
  let col_str = String.concat "\n" col_annotations in
  if col_str <> "" then
    print_function ("Columns:\n" ^ col_str) [ ANSITerminal.cyan ]
  else print_function "Table is empty." [ ANSITerminal.red ]

let load_db (name : string) =
  let _ = current_database := read_file name in
  let _ = current_file := name in
  print_function "Current database updated." [ ANSITerminal.red ]

let describe_tbls () =
  let tbls = List.map (fun tbl -> table_title tbl) !current_database in
  let tbls_str = String.concat "\n" tbls in
  if tbls_str <> "" then
    print_function ("Tables:\n" ^ tbls_str) [ ANSITerminal.cyan ]
  else print_function "Database is empty." [ ANSITerminal.red ]

let drop_tbl (name : string) =
  let updated = drop_tbl name !current_database in
  let _ = current_database := updated in
  print_function ("Dropped table " ^ name) [ ANSITerminal.cyan ]

let count_tbl (name : string) =
  let sz = count_tbl name !current_database in
  print_function ("COUNT:" ^ string_of_int sz) [ ANSITerminal.cyan ]

(* [capitalize s] capitalizes all letters found in [s]. Example: capitalize
   "uppercase" -> "UPPERCASE" *)
let capitalize s =
  String.fold_left
    (fun acc c -> acc ^ Char.(c |> uppercase_ascii |> escaped))
    "" s

let insert_into (vals : string list) =
  if List.length vals < 3 then raise Malformed
  else if List.nth (List.rev vals) 1 |> capitalize <> "INTO" then
    raise Malformed
  else
    let tbl_name = List.hd (List.rev vals) in
    match cols_of_table tbl_name !current_database with
    | cols ->
        let primitive_lst =
          vals
          |> List.filter (fun e -> capitalize e <> "INTO" && e <> tbl_name)
          |> List.map (fun s ->
                 String.fold_left
                   (fun acc c -> if c = ';' then acc else acc ^ Char.escaped c)
                   "" s)
        in
        let vals = primitive_to_values primitive_lst cols [] in
        let tbl = find_table tbl_name !current_database in
        let new_tbl = insert_row vals tbl in
        let new_db = update_tbl new_tbl !current_database in
        let _ = current_database := new_db in
        let _ = save !current_database (!current_file ^ ".json") in
        print_function "Added value to column" [ ANSITerminal.cyan ]

let add_col (vals : string list) =
  if List.nth vals 2 |> String.uppercase_ascii <> "TO" then raise Malformed
  else
    let tbl_name = List.hd (List.rev vals) in
    let col_name = List.hd vals in
    let col_type = List.nth vals 1 in
    let new_tbl =
      init_col col_name col_type (find_table tbl_name !current_database)
    in
    let new_db = update_tbl new_tbl !current_database in
    let _ = current_database := new_db in
    let _ = save !current_database (!current_file ^ ".json") in
    print_function
      ("Added column " ^ col_name ^ ":" ^ col_type ^ " to table " ^ tbl_name)
      [ ANSITerminal.cyan ]

let quit () =
  print_function "Quitting..." [ ANSITerminal.blue ];
  Stdlib.exit 0

let print_table (name : string) =
  if not (String.contains name '.') then
    let tbl = find_table name !current_database in
    print_table tbl
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let max_width = max_width tbl.cols 0 in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c -> print_col c max_width

let push () =
  save_to_cloud !current_database;
  print_function "Pushed..." [ ANSITerminal.blue ]

let save_csv (name : string) =
  let tbl = find_table name !current_database in
  save_csv tbl;
  print_function ("Saved: ~/Users/" ^ name ^ ".csv") [ ANSITerminal.blue ]

let sum (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Sum of " ^ name ^ " :" ^ string_of_float (sum c))
            [ ANSITerminal.blue ]

let mean (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Mean of " ^ name ^ " :" ^ string_of_float (mean c))
            [ ANSITerminal.blue ]

let max (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Max of " ^ name ^ " :" ^ string_of_float (max c))
            [ ANSITerminal.blue ]

let min (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Min of " ^ name ^ " :" ^ string_of_float (min c))
            [ ANSITerminal.blue ]

let pull (name : string) =
  let db = download name in
  current_database := db;
  save !current_database "db.json";
  print_function "Pulled from remote and updated db.json" [ ANSITerminal.blue ]

let find_all (vals : string list) =
  if List.length vals <> 3 then raise Malformed
  else if List.nth vals 1 |> String.uppercase_ascii <> "IN" then raise Malformed
  else
    let tbl_name = List.hd (List.rev vals) in
    let key = int_of_string (List.hd vals) in
    let tbl = find_table tbl_name !current_database in
    let vals = find_prim key tbl in
    print_function (String.concat " | " vals) [ ANSITerminal.cyan ]

let find_median (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Median of " ^ name ^ " :" ^ string_of_float (median c))
            [ ANSITerminal.blue ]

let find_variance (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Variance of " ^ name ^ " :" ^ string_of_float (variance c))
            [ ANSITerminal.blue ]

let find_dev (name : string) =
  if not (String.contains name '.') then raise Malformed
  else
    let lst = String.split_on_char '.' name in
    if List.length lst <> 2 then raise Malformed
    else
      let tbl_name = List.hd lst in
      let tbl = find_table tbl_name !current_database in
      let col_name = List.hd (List.rev lst) in
      let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
      match col with
      | None -> raise InvalidColumn
      | Some c ->
          print_function
            ("Standard Deviation of " ^ name ^ " :"
            ^ string_of_float (std_dev c))
            [ ANSITerminal.blue ]

let load_csv (vals : string list) =
  if List.length vals <> 1 then raise Malformed
  else
    let fname = List.hd vals in
    if
      table_exists
        Str.(global_replace (regexp {|\.|}) "_" fname)
        !current_database
    then raise TableExists
    else
      let table = load fname in
      let new_db = table :: !current_database in
      current_database := new_db;
      save new_db (!current_file ^ ".json");
      print_function
        ("Loaded table " ^ Str.(global_replace (regexp {|\.|}) "_" fname))
        [ ANSITerminal.blue ]

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let filter_col_int col predicate =
  let removed_indices = ref [] in
  let values =
    List.filter
      (fun elt ->
        match elt with
        | VInt i ->
            if predicate i then (
              let removed_index = find (VInt i) col.values in
              removed_indices := removed_index :: !removed_indices;
              predicate i)
            else predicate i
        | _ -> failwith "Impossible.")
      col.values
  in
  ({ name = col.name; col_type = col.col_type; values }, removed_indices)

let filter_col_str col predicate =
  let removed_indices = ref [] in
  let values =
    List.filter
      (fun elt ->
        match elt with
        | VString s ->
            if predicate s then (
              let removed_index = find (VString s) col.values in
              removed_indices := removed_index :: !removed_indices;
              predicate s)
            else predicate s
        | _ -> failwith "Impossible.")
      col.values
  in
  ({ name = col.name; col_type = col.col_type; values }, removed_indices)

let filter_col_flt col predicate =
  let removed_indices = ref [] in
  let values =
    List.filter
      (fun elt ->
        match elt with
        | VFloat s ->
            if predicate s then (
              let removed_index = find (VFloat s) col.values in
              removed_indices := removed_index :: !removed_indices;
              predicate s)
            else predicate s
        | _ -> failwith "Impossible.")
      col.values
  in
  ({ name = col.name; col_type = col.col_type; values }, removed_indices)

let filter_col_bl col predicate =
  let removed_indices = ref [] in
  let values =
    List.filter
      (fun elt ->
        match elt with
        | VBool s ->
            if predicate s then (
              let removed_index = find (VBool s) col.values in
              removed_indices := removed_index :: !removed_indices;
              predicate s)
            else predicate s
        | _ -> failwith "Impossible.")
      col.values
  in
  ({ name = col.name; col_type = col.col_type; values }, removed_indices)

let rec filter_cols cols indices acc =
  match cols with
  | [] -> acc
  | h :: t ->
      let filtered =
        List.filteri
          (fun idx _ -> List.exists (fun lstelt -> lstelt = idx) indices)
          h.values
      in
      let new_col =
        { col_type = h.col_type; values = filtered; name = h.name }
      in
      filter_cols t indices (new_col :: acc)

let int_helper c value op tbl =
  let predicate elt = op elt (int_of_string value) in
  let _, removed_indices = filter_col_int c predicate in
  let new_cols = filter_cols tbl.cols !removed_indices [] in
  let new_table = { title = tbl.title; cols = new_cols } in
  Printer.print_table new_table

let flt_helper c value op tbl =
  let predicate elt = op elt (float_of_string value) in
  let _, removed_indices = filter_col_flt c predicate in
  let new_cols = filter_cols tbl.cols !removed_indices [] in
  let new_table = { title = tbl.title; cols = new_cols } in
  Printer.print_table new_table

let string_helper c value op tbl =
  let predicate elt = op elt value in
  let _, removed_indices = filter_col_str c predicate in
  let new_cols = filter_cols tbl.cols !removed_indices [] in
  let new_table = { title = tbl.title; cols = new_cols } in
  Printer.print_table new_table

let bool_helper c value op tbl =
  let predicate elt = op elt (bool_of_string value) in
  let _, removed_indices = filter_col_bl c predicate in
  let new_cols = filter_cols tbl.cols !removed_indices [] in
  let new_table = { title = tbl.title; cols = new_cols } in
  Printer.print_table new_table

let find_where (vals : string list) =
  if List.length vals <> 5 then raise Malformed
  else
    let tbl_name = List.hd (List.rev vals) in
    let tbl = find_table tbl_name !current_database in
    let col_name = List.hd vals in
    let col = List.find_opt (fun elt -> elt.name = col_name) tbl.cols in
    match col with
    | None -> raise InvalidColumn
    | Some c -> (
        let op = List.nth vals 1 in
        let value = List.nth vals 2 in
        match c.col_type with
        | TInt -> (
            match op with
            | "<" -> int_helper c value ( < ) tbl
            | ">" -> int_helper c value ( > ) tbl
            | "=" -> int_helper c value ( = ) tbl
            | "!=" -> int_helper c value ( <> ) tbl
            | "<=" -> int_helper c value ( <= ) tbl
            | ">=" -> int_helper c value ( >= ) tbl
            | _ -> raise Malformed)
        | TFloat -> (
            match op with
            | "<" -> flt_helper c value ( < ) tbl
            | ">" -> flt_helper c value ( > ) tbl
            | "=" -> flt_helper c value ( = ) tbl
            | "!=" -> flt_helper c value ( <> ) tbl
            | "<=" -> flt_helper c value ( <= ) tbl
            | ">=" -> flt_helper c value ( >= ) tbl
            | _ -> raise Malformed)
        | TString -> (
            match op with
            | "<" -> string_helper c value ( < ) tbl
            | ">" -> string_helper c value ( > ) tbl
            | "=" -> string_helper c value ( = ) tbl
            | "!=" -> string_helper c value ( <> ) tbl
            | "<=" -> string_helper c value ( <= ) tbl
            | ">=" -> string_helper c value ( >= ) tbl
            | _ -> raise Malformed)
        | TBool -> (
            match op with
            | "=" -> bool_helper c value ( = ) tbl
            | "!=" -> bool_helper c value ( > ) tbl
            | _ -> raise Malformed)
        | _ -> raise Malformed)
