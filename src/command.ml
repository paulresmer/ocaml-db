exception Empty
exception Malformed

type command =
  | Help
  | TableInit of string
  | DescribeCols of string
  | DescribeTbls
  | LoadDB of string
  | DropTbl of string
  | CountTbl of string
  | InsertRow of string list
  | AddCols of string list
  | Quit
  | PrintTbl of string
  | Push
  | SaveCSV of string
  | Sum of string
  | Mean of string
  | Median of string
  | Max of string
  | Min of string
  | Var of string
  | StdDev of string
  | Pull of string
  | FindPrim of string list
  | LoadCSV of string list

(** [check_for_empty elem] is [true] if [elem] is the empty string, [false]
    otherwise. *)
let check_for_empty elem = elem <> ""

(** [list_phrases str] is a [string list] repr. of [str] split on the [" "]
    character. *)
let list_phrases str =
  str |> String.split_on_char ' ' |> List.filter check_for_empty

(** [parse str] is a value of type [object_phrase] that is mapped from the input
    string [str] *)
let parse s =
  let sanitised_str = s |> String.trim in
  if sanitised_str = "HELP" then Help
  else
    match list_phrases sanitised_str with
    | h :: t -> begin
        match h with
        | "CREATE" -> TableInit (String.concat " " t)
        | "COLS" -> DescribeCols (String.concat " " t)
        | "LOAD" -> LoadDB (String.concat " " t)
        | "TBLS" -> DescribeTbls
        | "DROP" -> DropTbl (String.concat " " t)
        | "COUNT" -> CountTbl (String.concat " " t)
        | "INSERT" -> InsertRow t
        | "ADD" -> AddCols t
        | "QUIT" -> Quit
        | "PRINT" -> PrintTbl (String.concat " " t)
        | "PUSH" -> Push
        | "SAVECSV" -> SaveCSV (String.concat " " t)
        | "SUM" -> Sum (String.concat " " t)
        | "MEAN" -> Mean (String.concat " " t)
        | "MAX" -> Max (String.concat " " t)
        | "MIN" -> Min (String.concat " " t)
        | "PULL" -> Pull (String.concat " " t)
        | "FINDPRIM" -> FindPrim t
        | "MEDIAN" -> Median (String.concat " " t)
        | "VAR" -> Var (String.concat " " t)
        | "DEV" -> StdDev (String.concat " " t)
        | "LOADCSV" -> LoadCSV t
        | _ -> raise Empty
      end
    | [] -> raise Empty
