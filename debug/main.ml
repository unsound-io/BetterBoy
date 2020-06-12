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

let load_sav cart =
  let open Rresult.R.Infix in
  Fpath.of_string cart >>= fun path ->
  let spath = Fpath.set_ext ".sav" path in
  Bos.OS.File.read spath
  >>= fun content ->
  Ok (Bytes.of_string content)

let save_file (m : Machine.t) cart =
  let open Rresult.R.Infix in
  Fpath.of_string cart >>= fun path ->
  let spath = Fpath.set_ext ".sav" path in
  match Cartridge.dump_sram m.cartridge () with
  | Some sram ->
    Bos.OS.File.exists spath >>= fun exists ->
    (if exists then
      Bos.OS.File.truncate spath 0
    else
      Ok ())
   >>= fun () ->
   Bos.OS.File.write spath (Bytes.to_string sram)
  | None -> Ok ()

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
  let sav =
    match load_sav cartridge with
    | Ok s -> Some s
    | _ -> print_endline "no sav file"; None
  in
  let cfg = {
    Betterboy.Machine.sample_rate = 48000;
    sample_size = 2048;
    bios;
    rom = Some rom;
    sav;
  } in
  let machine = Machine.make cfg in
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
     | Sdl.Exit -> let _ = save_file t.machine cartridge in print_endline "dddexit"; exit 0
     | exn -> Printexc.to_string exn |> print_endline; Sdl.teardown t; exit 1
   );
    loop t
  in
  Ok (loop state)

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.cartridge $ Args.bootrom $ Args.sym, Args.info)
