type value =
  | VInt of int
  | VString of string
  | VFloat of float
  | VBool of bool

type col_type =
  | TInt
  | TString
  | TFloat
  | TBool

type column = {
  name : string;
  values : value list;
  col_type : col_type;
}

type table = {
  title : string;
  cols : column list;
}

type db = table list

(*Returns the string repr. of a value*)
val to_string : value -> string

(*Returns the string repr. of a value type*)
val type_to_string : col_type -> string

val from_int : int -> value
(**Following functions construct values of our custom types*)

val from_float : float -> value
val from_string : string -> value
val from_bool : bool -> value
