open Command
open Destringify
open Db
open Dbtype
open Stringify

let current_file = ref "db"
let current_database = ref (read_file !current_file)

let print_function msg color =
  let _ = msg |> ANSITerminal.print_string color in
  ()

let help () =
  print_function
    "Available commands\n\
     CREATE t: create a new,empty table with name t.\n\
     COLS t: Print the columns of table t\n\
     LOAD n: Load file n.json as the current database.\n\
     TBLS: Display current tables.\n\
     DROP t: Drop table t from the db.\n\
     COUNT t: Display number of rows in table t.\n\
     ADD col TYPE to t: Add a new column col of type T YPE to table t\n\
     INSERT x1;...;xn INTO t: Add a new row to table t\n\n"
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

let insert_into (vals : string list) =
  if List.nth vals 1 |> String.capitalize_ascii <> "INTO" then raise Malformed
  else
    let tbl_name = List.hd (List.rev vals) in
    match cols_of_table tbl_name !current_database with
    | cols ->
        let primitive_lst =
          List.hd vals |> String.split_on_char ';'
          |> List.filter (fun elt -> elt <> "")
        in
        let vals = primitive_to_values primitive_lst cols [] in
        let tbl = find_table tbl_name !current_database in
        let new_tbl = insert_row vals tbl in
        let new_db = update_tbl new_tbl !current_database in
        let _ = current_database := new_db in
        let _ = save !current_database !current_file in
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
    let _ = save !current_database !current_file in
    print_function
      ("Added column " ^ col_name ^ ":" ^ col_type ^ " to table " ^ tbl_name)
      [ ANSITerminal.cyan ]