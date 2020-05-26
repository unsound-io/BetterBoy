open Nottui

let run m =
  Lwd.set Disassembler.cur m.Betterboy.Machine.cpu.pc;
  Nottui_widgets.tabs [
    "shell", (fun () -> Lwd_utils.pack Ui.pack_y (Shell.make ()));
    "disassembler", (fun () -> Disassembler.disassemble m);
    "vram viewer", (fun () -> Vram.vram () |> Lwd.join);
  ]
  |> Ui_loop.run
