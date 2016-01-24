open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Websocket_lwt
open Entities
open Events


let section = Lwt_log.Section.make "discord"
let dlog msg = Lwt_log.debug ~section ("[D] " ^ msg)
let ilog msg = Lwt_log.info ~section ("[I] " ^ msg)


let handle_json_response (resp, body) =
  match resp |> Response.status |> Code.code_of_status with
  | 200 -> body |> Cohttp_lwt_body.to_string >|= Yojson.Basic.from_string
  | code -> failwith ("Unexpected response code: " ^ (string_of_int code))

let fail_badresp resp_json =
  failwith ("Unexpected response: " ^ (Yojson.Basic.pretty_to_string resp_json))


let login_body email password =
  let json = `Assoc ["email", `String email; "password", `String password] in
  Cohttp_lwt_body.of_string (Yojson.Basic.to_string json)

let get_token api_base email password =
  let body = login_body email password in
  let headers = Header.of_list ["Content-type", "Application/json"] in
  Client.post ~body ~headers (Endpoints.login api_base) >>=
  handle_json_response >|= function
  | `Assoc [("token", `String token)] -> token
  | resp_json -> fail_badresp resp_json


let get_gateway api_base token =
  let headers = Header.of_list ["Authorization", token] in
  Client.get ~headers (Endpoints.gateway api_base) >>=
  handle_json_response >|= function
  | `Assoc [("url", `String gateway)] -> gateway
  | resp_json -> fail_badresp resp_json


module Message = struct
  let from_json json =
    Frame.create ~content:(Yojson.Basic.to_string json) ()

  let from_jlist jlist = from_json (`Assoc jlist)

  let make_first_frame token =
    from_jlist [
      "op", `Int 2;
      "d", `Assoc [
        "token", `String token;
        "v", `Int 3;
        "properties", `Assoc [
          "$os", `String "unknown";
          "$browser", `String "ocamlbot";
          "$device", `String "ocamlbot";
          "$referrer", `String "";
          "$referring_domain", `String "";
        ]]]
end


module Conn = struct
  type connection_state =
    | Disconnected
    | Connected

  type t = {
    token : string;
    gateway_uri : string;

    mutable send : Frame.t -> unit Lwt.t;
    mutable state : connection_state;

    mutable heartbeat_interval : float;
    mutable heartbeat_event : Lwt_engine.event option;

    mutable my_user_id : string;
    things : All_the_things.t;
  }

  let create token gateway_uri = {
    token;
    gateway_uri;

    send = (fun _ -> Lwt.return_unit);
    state = Disconnected;

    heartbeat_interval = 0.0;
    heartbeat_event = None;

    my_user_id = "";
    things = All_the_things.create ();
  }

  let connected conn send =
    conn.send <- send;
    conn.state <- Connected

  let send_frame conn fr =
    dlog ("send " ^ (Json_utils.prettify_or_escaped fr.Frame.content)) >>
    conn.send fr

  let send_jlist conn jlist =
    send_frame conn (Message.from_jlist jlist)

  let send_heartbeat conn =
    send_jlist conn [
      "op", `Int 1;
      "d", `String (string_of_int (int_of_float (Unix.time () *. 1000.)));
    ]

  let start_heartbeat conn hb_interval =
    conn.heartbeat_interval <- hb_interval;
    let heartbeat_timer_f _ = Lwt.async (fun () -> send_heartbeat conn) in
    let hev = Lwt_engine.on_timer hb_interval true heartbeat_timer_f in
    conn.heartbeat_event <- Some hev

  let close conn code =
    let send = conn.send in
    (match conn.heartbeat_event with
     | None -> ()
     | Some ev -> Lwt_engine.stop_event ev);
    conn.heartbeat_event <- None;
    conn.state <- Disconnected;
    conn.send <- (fun _ -> Lwt.return_unit);
    send (Frame.close code) >>
    Lwt.fail Exit

  let add_user conn ?(me=false) user =
    All_the_things.add_user conn.things user;
    if me then conn.my_user_id <- user.User.id

  let add_guild conn guild =
    All_the_things.add_guild conn.things guild

  let add_channel conn channel =
    All_the_things.add_channel conn.things channel

  let users_to_string conn =
    All_the_things.users_to_string conn.things

  let guilds_to_string conn =
    All_the_things.guilds_to_string conn.things

  let channels_to_string conn =
    All_the_things.channels_to_string conn.things

end


let populate_conn_guild conn guild_json =
  let guild = Guild.from_json guild_json in
  Conn.add_guild conn guild;
  let pcg_member member_json =
    Json_utils.member "user" member_json |> User.from_json |> Conn.add_user conn
  in
  Json_utils.get_list "members" guild_json |> List.iter pcg_member;
  let pcg_channel channel_json =
    Channel.from_json guild.Guild.id channel_json |> Conn.add_channel conn
  in
  Json_utils.get_list "channels" guild_json |> List.iter pcg_channel

let populate_conn conn d =
  let me = User.from_json (Json_utils.member "user" d) in
  Conn.add_user conn ~me:true me;
  Json_utils.get_list "guilds" d |>
  List.iter (fun guild -> populate_conn_guild conn guild)


let ev_ready conn d =
  ilog "Got READY message." >>= fun () ->
  let interval = Json_utils.get_float "heartbeat_interval" d /. 1000. in
  Conn.start_heartbeat conn interval;
  populate_conn conn d;
  ilog "Processed READY message." >>
  dlog ("users: " ^ Conn.users_to_string conn) >>
  dlog ("guilds: " ^ Conn.guilds_to_string conn) >>
  dlog ("channels: " ^ Conn.channels_to_string conn)


let ev_message_create conn d =
  ilog "Got MESSAGE_CREATE message." >>= fun () ->
  let cm_event = Events.Create_message.from_json d in
  ilog (Events.Create_message.to_string conn.Conn.things cm_event)


let dispatch_event conn msg_json =
  dlog ("recv " ^ Yojson.Basic.pretty_to_string msg_json) >>
  let op = Json_utils.get_int "op" msg_json
  and t = Json_utils.get_string "t" msg_json in
  match op, t with
  | 0, "READY" -> ev_ready conn (Json_utils.member "d" msg_json)
  | 0, "MESSAGE_CREATE" ->
    ev_message_create conn (Json_utils.member "d" msg_json)
  | 0, _ -> dlog ("Unknown message type " ^ t ^ ", ignoring.")
  | _, _ -> dlog ("Unknown op " ^ (string_of_int op) ^ ", ignoring.")


let pong = Frame.create ~opcode:Frame.Opcode.Pong ()


let decode_close_number fr =
  let high = int_of_char fr.Frame.content.[0] in
  let low = int_of_char fr.Frame.content.[1] in
  string_of_int ((high lsl 8) + low)


let process_frame conn fr =
  let open Frame in
  match fr.opcode with
  | Opcode.Ping -> dlog "WS ping" >> Conn.send_frame conn pong
  | Opcode.Pong -> dlog "WS pong"

  | Opcode.Text ->
    dispatch_event conn (Yojson.Basic.from_string fr.content)

  | Opcode.Close ->
    dlog ("close " ^ (decode_close_number fr)) >> Conn.close conn 1000

  | _ ->
    dlog ("recv unknown " ^ (String.escaped fr.content)) >> Conn.close conn 1002


let ws_to_http uri = "http" ^ String.sub uri 2 (String.length uri - 2)

let client conn =
  let uri = ws_to_http conn.Conn.gateway_uri |> Uri.of_string in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(
    endp_to_client ~ctx:default_ctx endp >>= fun client ->
    with_connection ~ctx:default_ctx client uri) >>= fun (recv, send) ->
  Conn.connected conn send;
  let react = process_frame conn in
  let rec react_forever () = recv () >>= react >>= react_forever in
  let rec pushf () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
      dlog "Got EOF." >> Conn.close conn 1000 >> pushf ()
    | Some content ->
      Conn.send_frame conn @@ Frame.create ~content () >> pushf ()
  in
  Conn.send_frame conn (Message.make_first_frame conn.Conn.token) >>
  pushf () <?> react_forever ()


let login ?(api_base=Endpoints.api_base) email password =
  get_token api_base email password >>= fun token ->
  get_gateway api_base token >>= fun gateway_uri ->
  dlog ("login token: " ^ token) >>
  dlog ("gateway url: " ^ gateway_uri) >>
  Lwt.return (token, gateway_uri)


let connect (token, gateway_uri) =
  client (Conn.create token gateway_uri)
