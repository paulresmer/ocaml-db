(* The following features of the database were tested using OUnit:

   1.creating a table into a database listing columns of a table 2.loading a
   json as a table into the database 3.listing current tables in a database
   4.dropping a table from a database 5.counting the number of entries inside a
   table

   These tests were developed using a black box testing approach, where we
   tested the functionality of the database without considering the internal
   implementation. The test cases were created by providing input to the
   database and verifying the output against the expected results.

   The following features were manually tested with REPL interaction:

   1. inserting a data entry into a table 2. loading a csv file as a table 3.
   pushing a database to the cloud 4. pulling a database from the cloud 5.
   calculating the mean of a column, if applicable 6. calculating the median of
   a column, if applicable 7. calculating the sum of a column, if applicable 8.
   calculating the maximum of a column, if applicable 9. calculating the minimum
   of a column, if applicable 10. querying a row against a primary key 11.
   querying a row that satisfy a predicate and 12. counting the number of rows
   that satisfy some predicate

   These tests were conducted manually by providing input to the database and
   verifying the output against the expected results.

   The testing approach used in this test plan demonstrates the correctness of
   the system because it covers a wide range of functionalities and provides a
   high level of confidence that the database works as intended. *)

open OUnit2
open SQLDB

(*sample values*)
let v1 : Dbtype.value = VInt 1
let v2 : Dbtype.value = VInt 2
let v3 : Dbtype.value = VString "a"
let v4 : Dbtype.value = VString "b"
let v5 : Dbtype.value = VBool false
let v6 : Dbtype.value = VFloat 3.4
let v7 : Dbtype.value = VFloat (-5.9)
let v8 : Dbtype.value = VBool true

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
  { name = "col6"; values = [ v3 ]; col_type = Dbtype.TString }

let _col8 : Dbtype.column =
  { name = "col8"; values = [ v5 ]; col_type = Dbtype.TBool }

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
let tbl9 : Dbtype.table = { title = "tbl9"; cols = [ col7 ] }

(*sample databases*)
let db1 : Dbtype.db = []
let db2 : Dbtype.db = [ tbl1 ]
let db3 : Dbtype.db = [ tbl1; tbl2 ]
let db4 : Dbtype.db = [ tbl2 ]
let db5 : Dbtype.db = [ tbl1; tbl2; tbl3 ]
let db6 : Dbtype.db = [ tbl2; tbl1; tbl3 ]

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
    destringify_test "destringify db2.json" db2 "db2";
    destringify_test "destringify db3.json" db3 "db3";
  ]

(*test Db*)
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

let update_tbl_test (name : string) (expected : Dbtype.db)
    (input_tbl : Dbtype.table) (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected
    (Db.update_tbl input_tbl input_db)
    ~printer:Stringify.stringify_db

let table_exists_test (name : string) (expected : bool)
    (input_tbl_name : string) (input_db : Dbtype.db) =
  name >:: fun _ ->
  assert_equal expected
    (Db.table_exists input_tbl_name input_db)
    ~printer:Bool.to_string

let db_tests =
  [
    col_name_test "name of col1 is \"col1\"" "col1" col1;
    col_name_test "name of col7 is \"\"" "" col7;
    get_col_test "get col1 from tbl2" col1 "col1" tbl2;
    get_col_test "get col1 from tbl2" col1 "col1" tbl2;
    get_col_test "get col1 from tbl3" col1 "col1" tbl3;
    get_col_test "get col2 from tbl3" col2 "col2" tbl3;
    get_col_test "get col7 from tbl9" col7 "" tbl9;
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
    update_tbl_test "update db1 to db2" db2 tbl1 db1;
    update_tbl_test "update db4 to db3" db3 tbl1 db4;
    update_tbl_test "update db2 to db2" db2 tbl1 db2;
    update_tbl_test "update db5 to db6" db6 tbl2 db5;
    table_exists_test "test table_exist for non-existent table" false "faketbl"
      db1;
    table_exists_test "test table_exist for existing table" true "tbl1" db2;
  ]

(*test Command*)

let parse_test (name : string) (expected : Command.command) (input : string) =
  name >:: fun _ -> assert_equal expected (Command.parse input)

let command_tests =
  [
    parse_test "HELP is HELP" Command.Help "HELP";
    parse_test "HELP is HELP" Command.Help "HELP";
  ]

(*test Dbtype*)

let type_string_test (name : string) (expected : string)
    (input_db : Dbtype.col_type) =
  name >:: fun _ -> assert_equal expected (Dbtype.type_to_string input_db)

let value_string_test (name : string) (expected : string)
    (input_val : Dbtype.value) =
  name >:: fun _ -> assert_equal expected (Dbtype.to_string input_val)

let dbtype_tests =
  [
    type_string_test "Int Column Type" "Int" col2.col_type;
    type_string_test "Int Column Type" "Int" col4.col_type;
    type_string_test "String Column Type" "String" col5.col_type;
    type_string_test "String Column Type" "String" col7.col_type;
    value_string_test "Int Value Type" "1" v1;
    value_string_test "Int Value Type" "2" v2;
    value_string_test "String Value Type" "a" v3;
    value_string_test "String Value Type" "b" v4;
    value_string_test "Float Value Type" "3.4" v6;
    value_string_test "Float Value Type (Negative)" "-5.9" v7;
    value_string_test "Bool Value Type (false)" "false" v5;
    value_string_test "Bool Value Type (true)" "true" v8;
  ]

let suite =
  ""
  >::: List.flatten
         [
           stringify_tests @ destringify_tests @ command_tests @ dbtype_tests
           @ db_tests @ command_tests;
         ]

let _ = run_test_tt_main suite
