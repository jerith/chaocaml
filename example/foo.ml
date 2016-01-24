open Cmdliner
open Lwt.Infix


let main api_base email password =
  Lwt_log.(add_rule "discord" Debug);
  Lwt_main.run (Discord.login ?api_base email password >>= Discord.connect)


(* Command line stuff. *)

let api_base =
  let doc = "alternate base URI for discord API" in
  Arg.(value & opt (some string) None & info ["api-base"] ~docv:"URI" ~doc)

let email =
  let doc = "discord account email address" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EMAIL" ~doc)

let password =
  let doc = "discord account password" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"PASSWORD" ~doc)


let () =
  let main_t = Term.(const main $ api_base $ email $ password) in
  let info = Term.info "foo" ~doc:"a discord bot" in
  match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
