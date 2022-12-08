open Dbtype

exception MalformedCSV
exception HeterogeneousCols

type csv = string list list

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(*Infers the type of a column and asserts type homogeneity*)
let infer_type (csv_column : string list) : col_type =
  if List.length csv_column < 2 then raise MalformedCSV
  else
    let infer_elt_type elt =
      if elt = "true" || elt = "false" then TBool
      else if Str.string_match (Str.regexp {|[0-9\.]+|}) elt 0 then TFloat
      else if Str.string_match (Str.regexp {|[0-9]+|}) elt 0 then TInt
      else TString
    in
    let first = infer_elt_type (List.nth csv_column 1) in
    let rec assert_homogeneity (csv_columns : string list) (acc : col_type) =
      match csv_columns with
      | [] -> acc
      | h :: t ->
          if infer_elt_type h <> acc then raise HeterogeneousCols
          else assert_homogeneity t acc
    in
    assert_homogeneity (List.tl csv_column) first

let rec extract_values (csv_column : string list) (col_type : col_type)
    (acc : value list) =
  match csv_column with
  | [] -> acc
  | h :: t -> begin
      try
        let acc =
          match col_type with
          | TInt -> VInt (int_of_string h) :: acc
          | TFloat -> VFloat (float_of_string h) :: acc
          | TBool -> VBool (bool_of_string h) :: acc
          | TString -> VString h :: acc
          | TPrim -> raise MalformedCSV
        in
        extract_values t col_type acc
      with Failure _ -> raise MalformedCSV
    end

let to_table (data : csv) (name : string) =
  if List.length data = 0 then raise MalformedCSV
  else
    let headers = List.hd data in
    let size = List.length headers in
    (*Assert that the nested list is not ragged.*)
    let rec assert_wellformed (size : int) (data : csv) =
      match data with
      | [] -> ()
      | h :: t ->
          if List.length h <> size then raise MalformedCSV
          else assert_wellformed size t
    in
    let _ = assert_wellformed size (List.tl data) in
    let tranposed = transpose data in
    let rec instantiate_columns (data : csv) (acc : column list) =
      match data with
      | [] -> acc
      | h :: t ->
          let col_type = infer_type h in
          let vals = extract_values (List.tl h) col_type [] in
          let name = List.hd h in
          let col = { col_type; name; values = vals } in
          instantiate_columns t (col :: acc)
    in
    let cols = instantiate_columns tranposed [] in
    { title = Str.(global_replace (regexp {|\.|}) "_" name); cols }

let load (fname : string) =
  try
    let data = Csv.load fname in
    to_table data fname
  with Csv.Failure _ -> raise MalformedCSV
