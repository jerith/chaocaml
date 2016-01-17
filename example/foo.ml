open Cmdliner


let main email password =
  Lwt_log.(add_rule "discord" Debug);
  Lwt_main.run (Discord.connect email password)


(* Command line stuff. *)

let email =
  let doc = "discord account email address" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EMAIL" ~doc)

let password =
  let doc = "discord account password" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"PASSWORD" ~doc)


let () =
  let main_t = Term.(const main $ email $ password) in
  let info = Term.info "foo" ~doc:"a discord bot" in
  match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
