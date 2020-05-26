open Common
open Betterboy

let load_file path =
  let open Rresult.R.Infix in
  Fpath.of_string path
  >>= Bos.OS.File.read
  >>= fun content ->
  Ok (Bytes.of_string content)

let run_debugger (t : Sdl.t) symfile =
  Lwd.set Disassembler.cur t.machine.cpu.pc;
  Lwd.set Common.machine t.machine;
  try
    Debugger.run symfile t.machine
  with
  | Resume ->
    t.breakpoints <- Lwd.peek breakpoints;
    t.paused <- false;
    Tsdl.Sdl.raise_window t.win
  | Quit -> Sdl.teardown t; exit 0
  | Step i ->
    t.step <- Some i;
    t.paused <- false;
    t.breakpoints <- Lwd.peek breakpoints;
    Tsdl.Sdl.raise_window t.win

let main cartridge bootrom sym =
  let open Rresult.R.Infix in
  load_file cartridge >>= fun rom ->
  (match bootrom with
  | Some bootrom -> load_file bootrom >>= fun s -> Ok (Some s)
  | None -> Ok None)
  >>= fun bios ->
  (match sym with
   | Some file ->
     load_file file >>= fun s -> Ok (Some (Symbols.parse (Bytes.to_string s)))
  | None -> Ok None)
  >>= fun sym ->
  let machine = Machine.make ?bios ~rom () in
  let state = Sdl.create machine in
  let () =
    match bios with
    | None -> Engine.initial_state state.machine
    | Some _ -> ()
  in
  let rec loop t =
    (try
       if t.Sdl.paused then
         run_debugger t sym
       else
         Sdl.step t;
    with
    | exn -> Printexc.to_string exn |> print_endline; Sdl.teardown t; exit 0
   );
    loop t
  in
  Ok (loop state)

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.cartridge $ Args.bootrom $ Args.sym, Args.info)
