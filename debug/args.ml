open Cmdliner

let sym =
  let doc = "sym file for the rom" in
  Arg.(value & opt (some string) None  & info ["S";"sym"] ~doc )

let cartridge =
  let doc = "cartridge to load in emulator" in
  Arg.(required & pos 0 (some string) None  & info [] ~doc )

let bootrom =
  let doc = "use this bootrom" in
  Arg.(value & opt (some string) None & info ["bootrom"; "b"] ~docv:"BREAKPOINTS" ~doc)

let info =
  let doc = "bork bork" in
  let man = [
    `S Manpage.s_bugs;
  ]
  in
Cmd.info "GoodBoy" ~version:"%‌%VERSION%%" ~doc ~exits:Cmd.Exit.defaults ~man
