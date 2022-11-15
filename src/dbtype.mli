(*A value of type [value] represents a value that can be added to a column in a
  table in a database*)
type value =
  | VInt of int
  | VString of string
  | VFloat of float
  | VBool of bool
  | VNull

(*A value of type [col_type] represents the types of value a column can have*)
type col_type =
  | TInt
  | TString
  | TFloat
  | TBool

(* A value of type [column] represents a column as a record with three fields:
   1. the name of the column 2. the list of values of the column 3. the type of
   the column Invariant: a column is a homogeneous data structure *)
type column = {
  name : string;
  values : value list;
  col_type : col_type;
}

(* A value of type [table] represents a table as a record with two fields: 1.
   the title of the table 2. the list of columns of the table *)
type table = {
  title : string;
  cols : column list;
}

(*A value of type [db] represents a database as a list of values of type
  [table]*)
type db = table list

(*[to_string v] is the string representation of value [v]*)
val to_string : value -> string

(*[to_string t] is the string representation of [col_type] [t]*)
val type_to_string : col_type -> string

(*[from_int i] is a value of type [value] representing the integer [i]*)
val from_int : int -> value

(*[from_float f] is a value of type [value] representing the float [f]*)
val from_float : float -> value

(*[from_string s] is a value of type [value] representing the string [s]*)
val from_string : string -> value

(*[from_bool b] is a value of type [value] representing the boolean [b]*)
val from_bool : bool -> value

(*[null ] creates a Null value *)
val null : value
