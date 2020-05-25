open Betterboy
    
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

let is_between' v (a, b) = v >= a && v <= b [@@inline]
  
let echo1          = 0xE000, 0xEFFF
let echo2          = 0xF000, 0xFDFF
let unused         = 0xFEA0, 0xFEFF
let cartridge_ram  = 0xA000, 0xBFFF

let registers = function
  | 0xFF04 -> "div" 
  | 0xFF05 -> "tima" 
  | 0xFF06 -> "tma"
  | 0xFF07 -> "tac"
  | 0xFF00 -> "joypad"
  | 0xFF40 -> "control"
  | 0xFF41 -> "lcd"
  | 0xFF42 -> "scr_y"
  | 0xFF43 -> "scr_x"
  | 0xFF44 -> "ly"
  | 0xFF45 -> "lyc"
  | 0xFF46 -> "dma_transfer"
  | 0xFF47 -> "bg_pal"
  | 0xFF48 -> "obj_pal0"
  | 0xFF49 -> "obj_pal1"
  | 0xFF4B -> "win_x"
  | 0xFF4A -> "win_y"
  | _ -> assert false

let regions (s : Machine.t) a =
  match a with
  | a when (is_between' a Machine.Bios.range) &&
           (s.cpu.in_bios = true) -> "BIOS"
  | a when is_between' a Machine.Cartridge.range || (is_between' a cartridge_ram)-> "CART"
  | a when is_between' a Machine.Vram.range -> "VRAM"
  | a when is_between' a Machine.Wram0.range -> "WRAM0"
  | a when is_between' a Machine.Wram1.range -> "WRAM1"
  | a when is_between' a echo1 -> "ECHO1"
  | a when is_between' a echo2 -> "ECHO2"
  | a when is_between' a unused -> "UNUSED"
  | a when is_between' a Machine.Hram.range -> "HRAM"
  | a when is_between' a Machine.Oam.range -> "OAM"
  | a when is_between' a Machine.Io_ports.range -> "IO"
  | _ -> "UNK"
