open OUnit2
open Lwt.Infix

let section = Lwt_log.Section.make "test_discord"
let dlog msg = Lwt_log.debug ~section ("[D] " ^ msg)


let api_port = ref 20000
let api_base () = "http://localhost:" ^ (string_of_int !api_port) ^ "/api"


let fping () =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  api_base () ^ "/fping" |> Uri.of_string |> Client.get >>= fun (resp, body) ->
  let status = resp |> Response.status |> Code.string_of_status in
  Cohttp_lwt_body.to_string body >>= fun body ->
  dlog (status ^ " :: " ^ body)


let ignore_cancel = function
  | Lwt.Canceled -> Lwt.return_unit
  | exn -> Lwt.fail exn

let run_with_fake_api srvinfo f =
  api_port := !api_port + 1;
  (* Lwt_log.(add_rule "discord" Debug); *)
  (* Lwt_log.(add_rule "fake_discord" Debug); *)
  (* Lwt_log.(add_rule "test_discord" Debug); *)
  let api_server = Fake_discord.api_server !api_port srvinfo in
  let stop_server () =
    (Lwt.cancel api_server; Lwt.catch (fun () -> api_server) ignore_cancel)
  in
  let wrap f = fun () -> f () >> Lwt.return (fun () -> ()) in
  let catch_fail exn = Lwt.return (fun () -> raise exn) in
  let t =
    Lwt.catch (wrap f) catch_fail >>= fun f ->
    dlog "foo" >>
    stop_server () >>
    Lwt.return f
  in
  let f = Lwt_main.run t in
  f ()


let assert_str ?msg a b = assert_equal ?msg ~printer:(fun x -> x) a b


let api_tests =
  "test_api" >::: [

    "test_login_valid" >:: (fun ctx ->
        let creds = ["foo@example.com", "password"] in
        let srvinfo = Fake_discord.make_srvinfo ~creds () in
        run_with_fake_api srvinfo (fun () ->
            Discord.login ~api_base:(api_base ()) "foo@example.com" "password"
            >>= fun (token, gateway_uri) ->
            (* The token is random, so we can't test it directly, but it's *)
            (* checked when we get the gateway. *)
            assert_str gateway_uri Fake_discord.default_gateway;
            Lwt.return_unit
          );
      );

  ]


let tests =
  "test_discord" >::: [
    api_tests;
  ]

let () = run_test_tt_main tests
