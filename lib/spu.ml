(* res: 
   https://emudev.de/gameboy-emulator/bleeding-ears-time-to-add-audio/
   https://www.reddit.com/r/EmuDev/comments/5gkwi5/gb_apu_sound_emulation/
   https://gbdev.gg8.se/wiki/index.php?title=Gameboy_sound_hardware
*)

module Square1 = struct

  open Machine.Square1
         
  let sweep_period { nr10; _ } = Uint8.((nr10 lsr (inj 4)) land (inj 0b111))
  let sweep_negate { nr10; _ } = Uint8.((nr10 land (inj 0b1000)) land (inj 0b1000))
  let sweep_shift { nr10; _ } = Uint8.(nr10 land (inj 0b111))

  let duty { nr11; _ } = Uint8.((nr11 lsr (inj 6)) land (inj 0b11))
  let length_load { nr11; _ } = Uint8.(nr11 land (inj 0b111111))

  (* NR12 FF12 VVVV APPP Starting volume, Envelope add mode, period
     NR13 FF13 FFFF FFFF Frequency LSB
     NR14 FF14 TL-- -FFF Trigger, Length enable, Frequency MSB 
  *)

  let starting_volume { nr12; _ } = Uint8.((nr12 lsr inj 4) land inj 0b111)
  let env_add_mode { nr12; _ } = Uint8.((nr12 lsr inj 3) land inj 0b1)
  let period { nr12; _ } = Uint8.(nr12 land inj 0b111)
                                  
  let frequency_lsb { nr13; _ } = nr13

  let trigger { nr14; _ } = Uint8.((nr14 lsr inj 6) land inj 0b1)
  let length_enable { nr14; _ } = Uint8.((nr14 lsr inj 5) land inj 0b1)
  let frequency_msb { nr14; _ } = Uint8.(nr14 land inj 0b111)
  

  (* duty lookup table:
     Duty   Waveform    Ratio
     -------------------------
     0      00000001    12.5%
     1      10000001    25%
     2      10000111    50%
     3      01111110    75% *)

  let duty_cycles = [|
    [| false; false; false; false; false; false; false; true; |];
    [| true; false; false; false; false; false; false; true; |];
    [| true; false; false; false; false; true; true; true; |];
    [| false; true; true; true; true; true; true; false; |];
  |]
 
  let get t = function
    | 0xFF10 -> t.nr10
    | 0xFF11 -> t.nr11
    | 0xFF12 -> t.nr12
    | 0xFF13 -> t.nr13
    | 0xFF14 -> t.nr14
    | _ -> assert false

  let set t addr v =
    match addr with
    | 0xFF10 -> t.nr10 <- v
    | 0xFF11 -> t.nr11 <- v
    | 0xFF12 -> t.nr12 <- v
    | 0xFF13 -> t.nr13 <- v
    | 0xFF14 -> t.nr14 <- v
    | _ -> assert false

  let step (t : t) =
    t.timer <- pred t.timer;
    if t.timer <= 0 then begin
      t.timer <- (2048 - t.timer_load) * 4; (* wtf *)
      t.sequence_step <- (succ t.sequence_step) land 0x7  
    end;

    if t.enabled && t.dac_enabled then
      t.out_volume <- t.volume
    else
      t.out_volume <- 0;

    if not duty_cycles.(Uint8.proj (duty t)).(t.sequence_step) then
      t.out_volume <- 0

 let length_tick (t : t) =
   if (t.length_count > 0 && ((length_enable t) = Uint8.one)) then begin
     t.length_count <- t.length_count - 1;
     if t.length_count = 0 then
       t.enabled <- false
   end

end 

(* let step (m : Machine.t) cycles =
 *   let rec aux cycles =
 *     if cycles = 0 then
 *       ()
 *     else begin
 *       
 *     end 
 *   in
 *   aux cycles *)
