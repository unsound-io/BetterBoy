open Nottui
open Lwd_infix

let top = Lwd.var (Lwd.return Ui.empty)
let bot = Lwd.var (Lwd.return Ui.empty)

let root =
  Lwd_utils.pack Ui.pack_y [
    Lwd.join (Lwd.get top);
    Lwd.join (Lwd.get bot)
  ]
    
let gravity_fill = Gravity.make ~h:`Negative ~v:`Negative

let gravity_crop = Gravity.make ~h:`Positive ~v:`Negative

let run m =
  top
  $= Lwd_utils.pack Ui.pack_x [ (* Disassembler.disassemble m; *)]; 
  bot
  $= Lwd_utils.pack Ui.pack_y (Shell.make ());

  Lwd.set Disassembler.cur m.Betterboy.Machine.cpu.pc;
  Ui_loop.run (Lwd.map' root (fun ui -> ui |> Ui.resize ~fill:gravity_fill ~crop:gravity_crop)) 
