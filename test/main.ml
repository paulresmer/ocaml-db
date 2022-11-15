open OUnit2
open SQLDB

(*sample values*)
let v1 : Dbtype.value = VInt 1
let v2 : Dbtype.value = VInt 2
let v3 : Dbtype.value = VString "a"
let v4 : Dbtype.value = VString "b"

(*sample columns*)
let col1 : Dbtype.column =
  { name = "col1"; values = []; col_type = Dbtype.TString }

let col2 : Dbtype.column =
  { name = "col2"; values = [ v1 ]; col_type = Dbtype.TInt }

let col3 : Dbtype.column =
  { name = "col3"; values = [ v2 ]; col_type = Dbtype.TInt }

let col4 : Dbtype.column =
  { name = "col4"; values = [ v1; v2 ]; col_type = Dbtype.TInt }

let col5 : Dbtype.column =
  { name = "col5"; values = [ v3; v4 ]; col_type = Dbtype.TString }

let col6 : Dbtype.column =
  { name = "col5"; values = [ v3 ]; col_type = Dbtype.TString }

let col7 : Dbtype.column = { name = ""; values = []; col_type = Dbtype.TString }

(*sample tables*)
let tbl1 : Dbtype.table = { title = "tbl1"; cols = [] }
let tbl2 : Dbtype.table = { title = "tbl2"; cols = [ col1 ] }
let tbl3 : Dbtype.table = { title = "tbl3"; cols = [ col1; col2 ] }
let _tbl4 : Dbtype.table = { title = "tbl4"; cols = [ col2 ] }
let _tbl5 : Dbtype.table = { title = "tbl5"; cols = [ col2; col3 ] }
let _tbl6 : Dbtype.table = { title = "tbl6"; cols = [ col2; col6 ] }
let _tbl7 : Dbtype.table = { title = "tbl7"; cols = [ col4; col5 ] }
let tbl8 : Dbtype.table = { title = ""; cols = [] }

(*sample databases*)
let db1 : Dbtype.db = []
let db2 : Dbtype.db = [ tbl1 ]
let db3 : Dbtype.db = [ tbl1; tbl2 ]
let db4 : Dbtype.db = [ tbl2 ]
let db5 : Dbtype.db = [ tbl1; tbl2; tbl3 ]

(*****************************************************************)
(* Test suite *)
(*****************************************************************)

(*test Stringify*)
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
      "{ \"name\": \"col4\", \"values\": [\"1\", \"2\"], \"type\": \"Int\"}"
      col4;
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

(*test Destringify*)
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

(*test Db*)
(*let insert_row_test (name : string) (expected : Dbtype.table) (input_row :
  Dbtype.value list) (input_table : Dbtype.table) = name >:: fun _ ->
  assert_equal expected (Db.insert_row input_row input_table)
  ~printer:Stringify.stringify_table*)
let col_name_test (name : string) (expected : string) (input : Dbtype.column) =
  name >:: fun _ ->
  assert_equal expected (Db.col_name input) ~printer:String.capitalize_ascii

let get_col_test (name : string) (expected : Dbtype.column) (input_req : string)
    (input_table : Dbtype.table) =
  name >:: fun _ ->
  assert_equal expected
    (Db.get_col input_req input_table)
    ~printer:Stringify.stringify_col

let rename_col_test (name : string) (expected : Dbtype.column)
    (input_rename : string) (input_col : Dbtype.column) =
  name >:: fun _ ->
  assert_equal expected
    (Db.rename_col input_rename input_col)
    ~printer:Stringify.stringify_col

let init_table_test (name : string) (expected : Dbtype.db) (input_name : string)
    (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected
    (Db.init_table input_name input_db)
    ~printer:Stringify.stringify_db

let cols_of_table_test (name : string) (expected : Dbtype.column list)
    (input_table_name : string) (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected (Db.cols_of_table input_table_name input_db)

let table_title_test (name : string) (expected : string) (input : Dbtype.table)
    =
  name >:: fun _ ->
  assert_equal expected (Db.table_title input) ~printer:String.capitalize_ascii

let retitle_tbl_test (name : string) (expected : Dbtype.table)
    (input_rename : string) (input_tbl : Dbtype.table) =
  name >:: fun _ ->
  assert_equal expected
    (Db.retitle_tbl input_rename input_tbl)
    ~printer:Stringify.stringify_table

let drop_tbl_test (name : string) (expected : Dbtype.db) (input_tbl : string)
    (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected
    (Db.drop_tbl input_tbl input_db)
    ~printer:Stringify.stringify_db

let count_tbl_test (name : string) (expected : int) (input_tbl : string)
    (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected (Db.count_tbl input_tbl input_db) ~printer:Int.to_string

let db_tests =
  [
    (*insert_row_test "insert empty row to empty table" tbl1 [] tbl1;
      insert_row_test "insert empty row to table" tbl2 [] tbl2; insert_row_test
      "insert row of length 1 to empty table" tbl4 [ v1 ] tbl1; insert_row_test
      "insert row of length 2 to empty table" tbl5 [ v1; v2 ] tbl1;
      insert_row_test "insert row of length 2 to table" tbl7 [ v2; v4 ] tbl6;*)
    col_name_test "name of col1 is \"col1\"" "col1" col1;
    col_name_test "name of col7 is \"\"" "" col7;
    get_col_test "get col1 from tbl2" col1 "col1" tbl2;
    get_col_test "get col1 from tbl2" col1 "col1" tbl2;
    get_col_test "get col1 from tbl3" col1 "col1" tbl3;
    get_col_test "get col2 from tbl3" col2 "col2" tbl3;
    rename_col_test "rename col1 with name \"\"" col7 "" col1;
    init_table_test "initialize empty table in db1" db2 "tbl1" db1;
    init_table_test "initialize empty table in db1" db3 "tbl1" db4;
    cols_of_table_test "columns of tbl1 are []" [] "tbl1" db2;
    cols_of_table_test "columns of tbl2 are [col1]" [ col1 ] "tbl2" db3;
    cols_of_table_test "columns of tbl3 are [col1; col2]" [ col1; col2 ] "tbl3"
      db5;
    table_title_test "title of tbl1 is \"tbl1\"" "tbl1" tbl1;
    table_title_test "title of tbl8 is \"\"" "" tbl8;
    retitle_tbl_test "retitle tbl1 with title \"\"" tbl8 "" tbl1;
    drop_tbl_test "db2 without tbl1 is db1" db1 "tbl1" db2;
    drop_tbl_test "db3 without tbl2 is db2" db2 "tbl2" db3;
    count_tbl_test "tbl1 in db2 has 0 columns" 0 "tbl1" db2;
    count_tbl_test "tbl2 in db3 has 1 column" 1 "tbl2" db3;
    count_tbl_test "tbl3 in db5 has 2 columns" 2 "tbl3" db5;
  ]

(*test Command*)
let command_tests = []

(*test Dbtype*)
let dbtype_tests = []

let suite =
  ""
  >::: List.flatten
         [
           stringify_tests @ destringify_tests @ command_tests @ dbtype_tests
           @ db_tests;
         ]

let _ = run_test_tt_main suite
