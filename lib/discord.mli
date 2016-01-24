(** [login ?api_base email password] makes API calls to get an auth token and
    the websocket gateway URI. *)
val login : ?api_base:string -> string -> string -> (string * string) Lwt.t

(** [connect (token, gateway_uri)] connects to the websocket gateway and does
    stuff. *)
val connect : (string * string) -> unit Lwt.t
