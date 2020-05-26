open Nottui

let run s m =
  Lwd.set Disassembler.cur m.Betterboy.Machine.cpu.pc;
  Nottui_widgets.tabs [
    "shell", (fun () -> Lwd_utils.pack Ui.pack_y (Shell.make ()));
    "disassembler", (fun () -> Disassembler.disassemble m s |> Lwd.join);
    "vram viewer", (fun () -> Vram.vram ());
  ]
  |> Ui_loop.run
