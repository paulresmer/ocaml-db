open Dbtype

let print_col (col : column) (max_width : int) =
  print_endline (String.make (max_width + 5) '*');
  print_endline
    ((col.name ^ ":"
     ^ type_to_string col.col_type
     ^ String.make
         (max_width + 1
         - String.length (col.name ^ ":" ^ type_to_string col.col_type)
         + 3)
         ' ')
    ^ "|");

  print_endline (String.make (max_width + 5) '*');
  let rec print_vals = function
    | [] -> ()
    | h :: t ->
        print_endline
          ((">> " ^ to_string h
           ^ String.make (max_width + 5 - (String.length (to_string h) + 4)) ' '
           )
          ^ "|");
        print_endline (String.make (max_width + 5) '-');
        print_vals t
  in
  print_vals col.values

let rec max_width (cols : column list) (acc : int) =
  match cols with
  | [] -> acc
  | h :: t ->
      let header_width =
        String.length (h.name ^ ":" ^ type_to_string h.col_type)
      in
      let width =
        List.fold_left max 0
          (List.map String.length (List.map (fun v -> to_string v) h.values))
      in

      if header_width > width then
        if header_width > acc then max_width t header_width else max_width t acc
      else if width > acc then max_width t width
      else max_width t acc

let print_table (table : table) =
  print_endline ("TABLE " ^ table.title);
  let max_width = max_width table.cols 0 in
  let rec print_cols = function
    | [] -> ()
    | h :: t ->
        print_col h max_width;
        print_cols t
  in
  print_cols table.cols