open Dbtype
open Db

let val_to_int = function
  | VInt i -> i
  | VNull -> 0
  | _ -> raise InvalidNumericColumn

let val_to_flt = function
  | VFloat i -> i
  | VNull -> 0.
  | _ -> raise InvalidNumericColumn

let map_vals_to_int (vals : value list) = List.map val_to_int vals
let map_vals_to_flt (vals : value list) = List.map val_to_flt vals

let sum (c : column) =
  match c.col_type with
  | TInt ->
      let int_values = map_vals_to_int c.values in
      float_of_int (List.fold_left ( + ) 0 int_values)
  | TFloat ->
      let float_values = map_vals_to_flt c.values in
      List.fold_left ( +. ) 0. float_values
  | _ -> raise InvalidNumericColumn

let mean (c : column) = sum c /. float_of_int (List.length c.values)

let max_int (c : column) =
  let int_values = map_vals_to_int c.values in
  let rec max_helper (lst : int list) (max : int) =
    match lst with
    | [] -> max
    | h :: t -> if h > max then max_helper t h else max_helper t max
  in
  max_helper int_values (c.values |> List.hd |> val_to_int)

let max_flt (c : column) =
  let float_values = map_vals_to_flt c.values in
  let rec max_helper (lst : float list) (max : float) =
    match lst with
    | [] -> max
    | h :: t -> if h > max then max_helper t h else max_helper t max
  in
  max_helper float_values (c.values |> List.hd |> val_to_flt)

let max (c : column) =
  match c.col_type with
  | TInt -> float_of_int (max_int c)
  | TFloat -> max_flt c
  | _ -> raise InvalidNumericColumn

let min_int (c : column) =
  let int_values = map_vals_to_int c.values in
  let rec min_helper (lst : int list) (max : int) =
    match lst with
    | [] -> max
    | h :: t -> if h < max then min_helper t h else min_helper t max
  in
  min_helper int_values (c.values |> List.hd |> val_to_int)

let min_flt (c : column) =
  let float_values = map_vals_to_flt c.values in
  let rec min_helper (lst : float list) (max : float) =
    match lst with
    | [] -> max
    | h :: t -> if h < max then min_helper t h else min_helper t max
  in
  min_helper float_values (c.values |> List.hd |> val_to_flt)

let min (c : column) =
  match c.col_type with
  | TInt -> float_of_int (min_int c)
  | TFloat -> min_flt c
  | _ -> raise InvalidNumericColumn

let median (c : column) =
  let values = map_vals_to_flt c.values in
  let sorted = List.sort Stdlib.compare values in
  if List.length sorted mod 2 = 1 then List.nth sorted (List.length sorted / 2)
  else
    List.nth sorted (List.length sorted / 2)
    +. (List.nth sorted ((List.length sorted / 2) - 1) /. 2.)

let variance (c : column) =
  let mean = mean c in
  let values =
    List.map
      (fun elt ->
        match elt with
        | VInt i -> float_of_int i
        | VFloat f -> f
        | VNull -> 0.
        | _ -> raise InvalidNumericColumn)
      c.values
  in
  let rec sum (lst : float list) (mean : float) (acc : float) =
    match lst with
    | [] -> acc
    | h :: t -> sum t mean (acc +. ((h -. mean) *. (h -. mean)))
  in
  sum values mean 0. /. (float_of_int (List.length values) -. 1.)

let std_dev (c : column) =
  let var = variance c in
  sqrt var