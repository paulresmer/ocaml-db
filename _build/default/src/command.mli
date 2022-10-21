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
  (* | SelectAll of string *)


val parse : string -> command
