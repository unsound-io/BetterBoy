module Cpu = struct

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

  let make () = {
    sp = 0;
    pc = 0;
    ime = false;
    t = 0;
    m = 0;
    a = Uint8.zero;
    b = Uint8.zero;
    c = Uint8.zero;
    d = Uint8.zero;
    e = Uint8.zero;
    h = Uint8.zero;
    l = Uint8.zero;
    z = false;
    n = false;
    hc = false;
    ca = false;
    halted = false;
    in_bios = true;
  }

end 

module Vram = struct
  
  type t = Addressable.t
  let range = 0x8000, 0x9FFF
  let make () = Addressable.make ~name:"vram" range
      
end

module Wram0 = struct
  
  type t = Addressable.t
  let range = 0xC000, 0xCFFF
  let make () = Addressable.make ~name:"wram0" range
      
end

module Wram1 = struct
  
  type t = Addressable.t
  let range = 0xD000, 0xDFFF
  let make () = Addressable.make ~name:"wram1" range
      
end

module Oam = struct
  
  type t = Addressable.t
  let range = 0xFE00, 0xFE9F
  let make () = Addressable.make ~name:"oam" range

end

module Io_ports = struct

  type t = Addressable.t
  let range = 0xFF00, 0xFF7F
  let make () = Addressable.make ~name:"io_ports" range

end

module Hram = struct

  type t = Addressable.t
  let range = 0xFF80, 0xFFFF
  let make () = Addressable.make ~name:"hram" range

end

module Bios = struct

  type t = Addressable.t
  let range = 0x0000, 0x00FE
  let make ?bios () =
    match bios with
    | Some buffer ->
      Addressable.make ~buffer ~name:"bios" range
    | None ->
      Addressable.make ~name:"bios" range

end

module Cartridge = struct 

  type t = Cartridge.t
  let range = 0x0000, 0x7FFF
  let make ?rom () = Cartridge.make ?rom () 
                       
end

module Timers = struct

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

  let make_rtc () = {
    high = Uint8.zero;
    low = Uint8.zero;
    seconds = Uint8.zero;
    minutes = Uint8.zero;
    hours = Uint8.zero;
    days = Uint8.zero;
  }
  
  let make () = {
    main = 0;
    sub = 0;
    div_c = 0;
    div = Uint8.zero;
    tima = Uint8.zero;
    tma = Uint8.zero;
    tac = Uint8.zero;
    rtc_real = None;
    rtc_latched = None;
    last_rtc = 0;
  }

end 

module Gpu = struct

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

  let make () = {
    clock = 0;
    dma_clock = 0;
    dma_transfer_request = false;
    redraw = false;
    lcd_register = Uint8.inj 0x80;
    control_register = Uint8.zero;
    ly = Uint8.zero;
    lyc = Uint8.zero;
    scr_y = Uint8.zero;
    scr_x = Uint8.zero;
    win_x = Uint8.zero;
    win_y = Uint8.zero;
    bg_pal = Uint8.zero;
    obj_pal0 = Uint8.zero;
    obj_pal1 = Uint8.zero;
    dma_transfer = Uint8.zero;
    framebuffer = Array.make_matrix 160 144 `Black;
    retrace_ly = 0;
  }

end 

module Joypad = struct

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

let make () = {
  control = Uint8.inj 0b11000000;
  arrows = Uint8.inj 0b1111;
  buttons =  Uint8.inj 0b1111;
}

end

module Square1 = struct

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

  let make () = {
    nr10 = Uint8.zero;
    nr11 = Uint8.zero;
    nr12 = Uint8.zero;
    nr13 = Uint8.zero;
    nr14 = Uint8.zero;
    timer = 0;
    timer_load = 0;
    sequence_step = 0;
    volume = 0;
    out_volume = 0;
    enabled = false;
    dac_enabled = false;
    length_count = 0;  
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

let make ?rom ?bios () = {
  cpu = Cpu.make ();
  gpu = Gpu.make ();
  timers = Timers.make ();
  vram = Vram.make ();
  wram0 = Wram0.make ();
  wram1 = Wram1.make ();
  bios = Bios.make ?bios ();
  io_ports = Io_ports.make ();
  hram = Hram.make ();
  oam = Oam.make ();
  joypad = Joypad.make ();
  cartridge = Cartridge.make ?rom ();
  serial = None;
  sqr1 = Square1.make ();
}
