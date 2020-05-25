module Cpu : sig

  type t = {
    mutable sp : int;
    mutable pc : int;
    mutable ime : bool;
    mutable t : int;
    mutable m : int;
    mutable a : Uint8.t;
    mutable b : Uint8.t;
    mutable c : Uint8.t;
    mutable d : Uint8.t;
    mutable e : Uint8.t;
    mutable h : Uint8.t;
    mutable l : Uint8.t;
    mutable z : bool;
    mutable n : bool;
    mutable hc : bool;
    mutable ca : bool;
    mutable halted : bool;
    mutable in_bios : bool;
  }

  val make : unit -> t
    
end

module Vram : sig
  type t = Addressable.t
  val range : (int * int)
end 

module Wram0 : sig
  type t = Addressable.t
  val range : (int * int)
end

module Wram1 : sig
  type t = Addressable.t
  val range : (int * int)
end 

module Oam : sig
  type t = Addressable.t
  val range : (int * int)
end

module Io_ports : sig
  type t = Addressable.t
  val range : (int * int)
end

module Hram : sig
  type t = Addressable.t
  val range : (int * int)
end

module Bios : sig
  type t = Addressable.t
  val range : (int * int)
end

module Cartridge : sig 
  type t = Cartridge.t
  val range : (int * int)
end

module Timers : sig

  type rtc = {
    mutable high : Uint8.t;
    mutable low : Uint8.t;
    mutable seconds : Uint8.t;
    mutable minutes : Uint8.t;
    mutable hours : Uint8.t;
    mutable days : Uint8.t;
  }
  
  type t = {
    mutable main : int;
    mutable sub : int;
    mutable div_c : int;
    mutable tima : Uint8.t;
    mutable div : Uint8.t;
    mutable tma : Uint8.t;
    mutable tac : Uint8.t;
    mutable last_rtc : int;
    mutable rtc_real : rtc option;
    mutable rtc_latched : rtc option;
  }

  val make_rtc : unit -> rtc
    
end 

module Gpu : sig

  type color = [ `White | `Black | `Light_gray | `Dark_gray ]
  type tile = color list

  type t = {
    mutable clock : int;
    mutable lcd_register : Uint8.t;
    mutable control_register : Uint8.t;
    mutable scr_y : Uint8.t;
    mutable scr_x : Uint8.t;
    mutable ly : Uint8.t;
    mutable lyc : Uint8.t;
    mutable win_x : Uint8.t;
    mutable win_y : Uint8.t;
    mutable bg_pal  : Uint8.t;
    mutable obj_pal0 : Uint8.t;
    mutable obj_pal1 : Uint8.t;
    mutable dma_transfer : Uint8.t;
    mutable dma_transfer_request : bool;
    mutable retrace_ly : int;
    mutable redraw : bool;
    mutable dma_clock : int;
    framebuffer : color array array;
  }

end 

module Joypad : sig

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

end

module Square1 : sig

  type t = {
    mutable nr10 : Uint8.t; (* 0xFF10 -PPP NSSS  Sweep period, negate, shift *)
    mutable nr11 : Uint8.t; (* 0xFF11 DDLL LLLL  Duty, Length load (64-L) *)
    mutable nr12 : Uint8.t; (* 0xFF12 VVVV APPP  Starting volume, Envelope add mode, period  *)
    mutable nr13 : Uint8.t; (* 0xFF13 FFFF FFFF  Frequency LSB *)
    mutable nr14 : Uint8.t; (* 0xFF14 TL-- -FFF  Trigger, Length enable, Frequency MSB *)
    mutable timer : int;
    mutable timer_load : int;
    mutable sequence_step : int;
    mutable volume : int;
    mutable out_volume : int;
    mutable enabled : bool;
    mutable dac_enabled : bool;
    mutable length_count : int;
  }

end

type t = {
  cpu : Cpu.t;
  timers : Timers.t;
  gpu : Gpu.t;
  vram : Vram.t;
  wram0 : Wram0.t;
  wram1 : Wram1.t;
  bios : Bios.t;
  io_ports : Io_ports.t;
  hram : Hram.t;
  oam : Oam.t;
  cartridge : Cartridge.t;
  joypad : Joypad.t;
  sqr1 : Square1.t;
  mutable serial : Uint8.t option;
}

val make : ?rom:bytes -> ?bios:bytes -> unit -> t
