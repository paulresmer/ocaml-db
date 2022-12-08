open Lwt
open Cohttp
open Cohttp_lwt_unix
open Stringify
open Destringify

let key = "$2b$10$21oSsbY8XbKpuJ3v3KI87.S16wOOsGJVEkTqQUE9IaxYuHgTCD9lq"

(** [print_function msg color func] prints string [msg] in color [color] and
    then calls function [func] after the printing is complete *)
let print_function msg color =
  let _ = msg |> ANSITerminal.print_string color in
  ()

let save_to_cloud db =
  let to_string = stringify_db db in
  let headers = Header.init () in
  let content_type = Header.add headers "Content-Type" "Application/JSON" in
  let auth_key = Header.add content_type "X-Master-Key" key in
  let priv_header = Header.add auth_key "X-Bin-Private" "false" in
  let request =
    Client.post
      (Uri.of_string "https://api.jsonbin.io/v3/b")
      ~body:(Cohttp_lwt.Body.of_string to_string)
      ~headers:priv_header
    >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let json = Yojson.Basic.from_string body in
    let assoc = Yojson.Basic.Util.to_assoc json in
    let metadata = List.assoc "metadata" assoc in
    let metadata_assoc = Yojson.Basic.Util.to_assoc metadata in
    let id = List.assoc "id" metadata_assoc in
    let id_string = Yojson.Basic.to_string id in
    let clean = Str.(global_replace (regexp "\"") "" id_string) in
    let id = "ID: " ^ clean in
    print_function id [ ANSITerminal.red ];
    print_endline ""
  in
  Lwt_main.run request

let download id =
  let headers = Header.init () in
  let content_type = Header.add headers "Content-Type" "Application/JSON" in
  let auth_key = Header.add content_type "X-Master-Key" key in
  let request =
    Client.get
      (Uri.of_string ("https://api.jsonbin.io/v3/b/" ^ id))
      ~headers:auth_key
    >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body -> parse_string body
  in
  Lwt_main.run request
