open Dbtype

type csv = string list list

let headers (table : table) =
  let rec iterate (cols : column list) (acc : string list) =
    match cols with
    | [] -> acc
    | h :: t -> iterate t (h.name :: acc)
  in
  iterate table.cols []

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let to_csv (table : table) =
  let header_row = headers table in
  let cols = table.cols in
  let values = List.map (fun col -> col.values) cols in
  let tranposed = transpose values in
  let string_vals =
    List.map
      (fun value_list -> List.map (fun elt -> to_string elt) value_list)
      tranposed
  in
  List.rev header_row :: string_vals

let save_csv (table : table) =
  let data = to_csv table in
  Csv.save ("../" ^ table.title ^ ".csv") data
