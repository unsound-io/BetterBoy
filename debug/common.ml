exception Resume
exception Quit
exception Step of int
    
module Breakpoints = Set.Make(Int)

let breakpoints = Lwd.var Breakpoints.empty
let machine = Lwd.var (Betterboy.Machine.make ())
