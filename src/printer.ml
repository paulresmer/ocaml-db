open Dbtype

let print_col (col : column) =
  print_endline ("Column " ^ col.name ^ " : " ^ type_to_string col.col_type);
  print_endline "********";
  let rec print_vals = function
    | [] -> ()
    | h :: t ->
        print_endline (">> " ^ to_string h);
        print_endline "----------";
        print_vals t
  in
  print_vals col.values

let print_table (table : table) =
  print_endline ("TABLE " ^ table.title);
  let rec print_cols = function
    | [] -> ()
    | h :: t ->
        print_col h;
        print_cols t
  in
  print_cols table.cols