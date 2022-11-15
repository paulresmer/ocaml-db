type value =
  | VInt of int
  | VString of string
  | VFloat of float
  | VBool of bool
  | VNull

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

let to_string = function
  | VInt i -> string_of_int i
  | VString s -> s
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VNull -> "NULL"

let type_to_string = function
  | TInt -> "Int"
  | TString -> "String"
  | TFloat -> "Float"
  | TBool -> "Bool"

let from_int i = VInt i
let from_string s = VString s
let from_float f = VFloat f
let from_bool b = VBool b
let null = VNull