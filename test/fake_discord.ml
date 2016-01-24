open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

type t = {
  base_path : string;
  gateway : string;
  creds : (string * string) list;
  mutable tokens : string list;
}

let default_base_path = "/api"
let default_gateway = "ws://localhost:8002/1"

let make_srvinfo
    ?(base_path=default_base_path)
    ?(gateway=default_gateway)
    ?(creds=[])
    ?(tokens=[])
    ()= {
  base_path;
  gateway;
  creds;
  tokens;
}


let section = Lwt_log.Section.make "fake_discord"
let dlog msg = Lwt_log.debug ~section ("[D] " ^ msg)
let ilog msg = Lwt_log.info ~section ("[I] " ^ msg)
let wlog msg = Lwt_log.warning ~section ("[W] " ^ msg)


let bad_request ?(status=`Bad_request) text =
  let body = Yojson.Basic.to_string (`Assoc ["message", `String text]) in
  wlog ("Bad request: " ^ text) >>
  Server.respond_string ~status ~body ()

let json_resp ?(status=`OK) jlist =
  let body = Yojson.Basic.to_string (`Assoc jlist) in
  Server.respond_string ~status ~body ()

let handle_not_basepath srvinfo req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  body |> Cohttp_lwt_body.to_string >|= (fun body ->
      Printf.sprintf "Bad path!\nUri: %s\nMethod: %s\nBody: %s"
        uri meth body) >>=
  bad_request

let handle_unknown path srvinfo req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  body |> Cohttp_lwt_body.to_string >|= (fun body ->
      Printf.sprintf "Path: %s\nUri: %s\nMethod: %s\nHeaders: %s\nBody: %s"
        path uri meth headers body) >>=
  bad_request ~status:`Not_found

let handle_fping srvinfo req body =
  body |> Cohttp_lwt_body.to_string >|= ignore >>
  dlog "pong" >> json_resp ["ping", `String "pong"]

let mktoken email password =
  let salt = string_of_int (Random.int 90 + 10) in
  Digest.string (salt ^ ":" ^ email ^ ":" ^ password) |> Digest.to_hex

let login srvinfo email password =
  match List.mem (email, password) srvinfo.creds with
  | true ->
    let token = mktoken email password in
    srvinfo.tokens <- token :: srvinfo.tokens;
    json_resp ["token", `String token]
  | false -> bad_request "Invalid username or password."

let handle_login srvinfo req body =
  body |> Cohttp_lwt_body.to_string >|= Yojson.Basic.from_string >>= function
  | `Assoc ["email", `String email; "password", `String password] ->
    login srvinfo email password
  | json -> bad_request ("Invalid body: " ^ (Yojson.Basic.to_string json))

let require_auth f srvinfo req body =
  match req |> Request.headers |> Header.get_authorization with
  | Some (`Other tok) when List.mem tok srvinfo.tokens -> f srvinfo req body
  | _ -> bad_request ~status:`Unauthorized ""

let handle_gateway srvinfo req body =
  body |> Cohttp_lwt_body.to_string >|= ignore >>
  json_resp ["url", `String srvinfo.gateway]

let get_path base_path req =
  let path = req |> Request.uri |> Uri.path in
  let bplen = String.length base_path in
  let prefix = String.sub path 0 bplen in
  match base_path = prefix with
  | false -> None
  | true -> Some (String.sub path bplen (String.length path - bplen))

let dispatch srvinfo _conn req body =
  let handle = match get_path srvinfo.base_path req with
    | None -> handle_not_basepath
    | Some "/fping" -> handle_fping
    | Some "/auth/login" -> handle_login
    | Some "/gateway" -> require_auth handle_gateway
    | Some path -> handle_unknown path
  in
  handle srvinfo req body


let api_server port srvinfo =
  let server = Server.make ~callback:(dispatch srvinfo) () in
  Server.create ~timeout:1 ~mode:(`TCP (`Port port)) server

let run_server () =
  Lwt_log.(add_rule "fake_discord" Debug);
  let creds = ["ocamlbot@mailinator.com", "password"] in
  let srvinfo = make_srvinfo ~creds () in
  Lwt_main.run @@ api_server 8001 srvinfo

(* let () = run_server () *)
