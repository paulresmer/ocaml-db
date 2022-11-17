open SQLDB.Command
open SQLDB.Destringify
open SQLDB.Db
open SQLDB.Exec
open SQLDB.Csv_read

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
        | Quit -> quit ()
        | PrintTbl t ->
            print_table t;
            main_repl ()
        | Push ->
            push ();
            main_repl ()
        | SaveCSV t ->
            save_csv t;
            main_repl ()
        | Sum t ->
            sum t;
            main_repl ()
        | Mean t ->
            mean t;
            main_repl ()
        | Max t ->
            max t;
            main_repl ()
        | Min t ->
            min t;
            main_repl ()
        | Pull t ->
            pull t;
            main_repl ()
        | FindPrim t ->
            find_all t;
            main_repl ()
        | Median t ->
            find_median t;
            main_repl ()
        | Var t ->
            find_variance t;
            main_repl ()
        | StdDev t ->
            find_dev t;
            main_repl ()
        | LoadCSV t ->
            load_csv t;
            main_repl ()
      with
      | Sys_error err -> print_function err [ ANSITerminal.red ] main_repl ()
      | Failure err ->
          print_function
            ("Invalid argument." ^ err)
            [ ANSITerminal.red ] main_repl ()
      | Invalid_argument _ ->
          print_function "Could not find a valid row with that id."
            [ ANSITerminal.red ] main_repl ()
      | Not_found ->
          print_function "Could not find a valid row with that id."
            [ ANSITerminal.red ] main_repl ()
      | InvalidFind ->
          print_function "Could not find a valid row with that id."
            [ ANSITerminal.red ] main_repl ()
      | ColumnValueMismatch ->
          print_function "Not enough values provided." [ ANSITerminal.red ]
            main_repl ()
      | InvalidNumericColumn ->
          print_function "Cannot sum over specified column."
            [ ANSITerminal.red ] main_repl ()
      | Malformed ->
          print_function "Invalid input. Enter HELP." [ ANSITerminal.red ]
            main_repl ()
      | Empty ->
          print_function "Empty input. Enter HELP." [ ANSITerminal.red ]
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
            ()
      | InvalidColumn ->
          print_function "Column does not exist." [ ANSITerminal.red ] main_repl
            ()
      | PrimaryColumnAlreadyExists ->
          print_function "Table already has a primary key." [ ANSITerminal.red ]
            main_repl ()
      | PrimaryKeyAlreadyExists ->
          print_function "A primary key with that value already exists"
            [ ANSITerminal.red ] main_repl ()
      | Yojson.Basic.Util.Type_error _ ->
          print_function "Not a valid remote id." [ ANSITerminal.red ] main_repl
            ()
      | TableExists ->
          print_function "A table with that name already exists."
            [ ANSITerminal.red ] main_repl ()
      | MalformedCSV ->
          print_function "Malformed CSV paassed in" [ ANSITerminal.red ]
            main_repl ())

(*run REPL loop*)
let () =
  let _ = load_default in
  main_repl ()
