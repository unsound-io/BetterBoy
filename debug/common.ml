exception Resume
exception Quit
exception Step of int

let cfg = {
  Betterboy.Machine.sample_rate = 48000;
  sample_size = 2048;
  bios = None;
  rom = None;
  sav = None;
}

module Breakpoints = Set.Make(Int)

let breakpoints = Lwd.var Breakpoints.empty
let machine = Lwd.var (Betterboy.Machine.make cfg)
