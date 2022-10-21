open SQLDB.Command
open SQLDB.Destringify
open SQLDB.Db

(** [print_function msg color func] prints string [msg] in color [color] and
    then calls function [func] after the printing is complete *)
let print_function msg color func =
  let _ = msg |> ANSITerminal.print_string color in
  func

let load_default =
  print_function "Loading default database db.json" [ ANSITerminal.cyan ]
    read_file "db"

let current_database = ref (read_file "db")

let rec main_repl () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nO-DBMShell>> ";
  match read_line () with
  | exception End_of_file -> ()
  | command_input -> (
      try
        match parse command_input with
        | Help ->
            print_function
              "Available commands\n\
               CREATE t: create a new,empty table with name t.\n\
               COLS t: Print the columns of table t\n\
               LOAD n: Load file n.json as the current database.\n\
               TBLS: Display current tables.\n\
               DROP t: Drop table t from the db.\n\
               COUNT t: Display number of rows in table t."
              [ ANSITerminal.cyan ] main_repl ()
        | TableInit t ->
            let _ = init_table t !current_database in
            print_function ("\nCreated table " ^ t) [ ANSITerminal.cyan ]
              main_repl ()
        | DescribeCols t ->
            let cols = cols_of_table t !current_database in
            let col_names = List.map (fun elt -> col_name elt) cols in
            let col_str = String.concat "\n" col_names in
            if col_str <> "" then
              print_function ("Columns:\n" ^ col_str) [ ANSITerminal.cyan ]
                main_repl ()
            else
              print_function "Table is empty." [ ANSITerminal.red ] main_repl ()
        | LoadDB t ->
            let _ = current_database := read_file t in
            print_function "Current database updated." [ ANSITerminal.red ]
              main_repl ()
        | DescribeTbls ->
            let tbls =
              List.map (fun tbl -> title_of_table tbl) !current_database
            in
            let tbls_str = String.concat "\n" tbls in
            if tbls_str <> "" then
              print_function ("Tables:\n" ^ tbls_str) [ ANSITerminal.cyan ]
                main_repl ()
            else
              print_function "Database is empty." [ ANSITerminal.red ] main_repl
                ()
        | DropTbl t ->
            let updated = drop_tbl t !current_database in
            let _ = current_database := updated in
            print_function ("Dropped table " ^ t) [ ANSITerminal.cyan ]
              main_repl ()
        | CountTbl t ->
            let sz = count_tbl t !current_database in
            print_function
              ("COUNT:" ^ string_of_int sz)
              [ ANSITerminal.cyan ] main_repl ()
      with
      | Empty ->
          print_function "Invalid input. Enter HELP." [ ANSITerminal.cyan ]
            main_repl ()
      | InvalidAdd ->
          print_function "Table already exists." [ ANSITerminal.red ] main_repl
            ()
      | InvalidTableName ->
          print_function "Table does not exist." [ ANSITerminal.red ] main_repl
            ()
      | InvalidDB ->
          print_function "Invalid file." [ ANSITerminal.red ] main_repl ())

(*run REPL loop*)
let () =
  let _ = load_default in
  main_repl ()
