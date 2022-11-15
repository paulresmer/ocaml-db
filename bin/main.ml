open SQLDB.Command
open SQLDB.Destringify
open SQLDB.Db
open SQLDB.Exec

(** [print_function msg color func] prints string [msg] in color [color] and
    then calls function [func] after the printing is complete *)
let print_function msg color func =
  let _ = msg |> ANSITerminal.print_string color in
  func

let load_default =
  print_function "Loading default database db.json" [ ANSITerminal.cyan ]
    read_file "db"

let rec main_repl () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nO-DBMShell>> ";
  match read_line () with
  | exception End_of_file -> ()
  | command_input -> (
      try
        match parse command_input with
        | Help ->
            help ();
            main_repl ()
        | TableInit t ->
            create_table t;
            main_repl ()
        | DescribeCols t ->
            describe_cols t;
            main_repl ()
        | LoadDB t ->
            load_db t;
            main_repl ()
        | DescribeTbls ->
            describe_tbls ();
            main_repl ()
        | DropTbl t ->
            drop_tbl t;
            main_repl ()
        | CountTbl t ->
            count_tbl t;
            main_repl ()
        | InsertRow t ->
            insert_into t;
            main_repl ()
        | AddCols t ->
            add_col t;
            main_repl ()
      with
      | ColumnValueMismatch ->
          print_function "Not enough values provided." [ ANSITerminal.red ]
            main_repl ()
      | Malformed ->
          print_function "Invalid input. Enter HELP." [ ANSITerminal.red ]
            main_repl ()
      | Empty ->
          print_function "Invalid input. Enter HELP." [ ANSITerminal.red ]
            main_repl ()
      | InvalidAdd ->
          print_function "Invalid add." [ ANSITerminal.red ] main_repl ()
      | InvalidInsert ->
          print_function "Invalid insert." [ ANSITerminal.red ] main_repl ()
      | InvalidTableName ->
          print_function "Table does not exist." [ ANSITerminal.red ] main_repl
            ()
      | InvalidDB ->
          print_function "Invalid file." [ ANSITerminal.red ] main_repl ()
      | InvalidColType ->
          print_function "Invalid column type." [ ANSITerminal.red ] main_repl
            ())

(*run REPL loop*)
let () =
  let _ = load_default in
  main_repl ()
