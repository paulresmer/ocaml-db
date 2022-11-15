open OUnit2
open SQLDB

(*sample values*)
let v1 : Dbtype.value = VInt 1
let v2 : Dbtype.value = VInt 2

(*sample columns*)
let col1 : Dbtype.column =
  { name = "col1"; values = []; col_type = Dbtype.TString }

let col2 : Dbtype.column =
  { name = "col2"; values = [ v1 ]; col_type = Dbtype.TInt }

let col3 : Dbtype.column =
  { name = "col3"; values = [ v1; v2 ]; col_type = Dbtype.TInt }

(*sample tables*)
let tbl1 : Dbtype.table = { title = "tbl1"; cols = [] }
let tbl2 : Dbtype.table = { title = "tbl2"; cols = [ col1 ] }
let tbl3 : Dbtype.table = { title = "tbl3"; cols = [ col1; col2 ] }

(*sample databases*)
let db1 : Dbtype.db = []
let db2 : Dbtype.db = [ tbl1 ]
let db3 : Dbtype.db = [ tbl1; tbl2 ]

(*****************************************************************)
(* Test suite *)
(*****************************************************************)

let stringify_col_test (name : string) (expected : string)
    (column : Dbtype.column) =
  name >:: fun _ ->
  assert_equal expected
    (Stringify.stringify_col column)
    ~printer:String.capitalize_ascii

let stringify_table_test (name : string) (expected : string)
    (table : Dbtype.table) =
  name >:: fun _ ->
  assert_equal expected
    (Stringify.stringify_table table)
    ~printer:String.capitalize_ascii

let stringify_db_test (name : string) (expected : string) (db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected
    (Stringify.stringify_db db)
    ~printer:String.capitalize_ascii

let stringify_tests =
  [
    stringify_col_test "sample col with no values"
      "{ \"name\": \"col1\", \"values\": [], \"type\": \"String\"}" col1;
    stringify_col_test "sample col with one value"
      "{ \"name\": \"col2\", \"values\": [\"1\"], \"type\": \"Int\"}" col2;
    stringify_col_test "sample col with two values"
      "{ \"name\": \"col3\", \"values\": [\"1\", \"2\"], \"type\": \"Int\"}"
      col3;
    stringify_table_test "sample table with no columns"
      "{ \"title\": \"tbl1\", \"columns\": []}" tbl1;
    stringify_table_test "sample table with one column"
      "{ \"title\": \"tbl2\", \"columns\": [{ \"name\": \"col1\", \"values\": \
       [], \"type\": \"String\"}]}"
      tbl2;
    stringify_table_test "sample table with two columns"
      "{ \"title\": \"tbl3\", \"columns\": [{ \"name\": \"col1\", \"values\": \
       [], \"type\": \"String\"}, { \"name\": \"col2\", \"values\": [\"1\"], \
       \"type\": \"Int\"}]}"
      tbl3;
    stringify_db_test "sample database with no tables" "{ \"tables\": []}" db1;
    stringify_db_test "sample database with one table"
      "{ \"tables\": [{ \"title\": \"tbl1\", \"columns\": []}]}" db2;
    stringify_db_test "sample database with two tables"
      "{ \"tables\": [{ \"title\": \"tbl1\", \"columns\": []}, { \"title\": \
       \"tbl2\", \"columns\": [{ \"name\": \"col1\", \"values\": [], \"type\": \
       \"String\"}]}]}"
      db3;
  ]

let destringify_test (name : string) (expected : Dbtype.db) (input : string) =
  name >:: fun _ ->
  assert_equal expected
    (Destringify.read_file input)
    ~printer:Stringify.stringify_db

let destringify_tests =
  [
    destringify_test "destringify db1.json" db1 "db1";
    destringify_test "destringify db.json" db2 "db2";
    destringify_test "destringify db.json" db3 "db3";
  ]

let suite = "" >::: List.flatten [ stringify_tests @ destringify_tests ]
let _ = run_test_tt_main suite
