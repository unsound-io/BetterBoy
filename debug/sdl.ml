open Common
open Betterboy
open Tsdl
open Result

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

type t = {
  machine : Machine.t;
  win : Tsdl.Sdl.window;
  event : Tsdl.Sdl.event;
  device_id : int32;
  mutable quit : bool;
  mutable last_tick : Int32.t;
  mutable last_poll : Int32.t;
  mutable paused : bool;
  mutable breakpoints : Breakpoints.t;
  mutable step : int option;
}

let teardown t =
  Sdl.destroy_window t.win;
  ()

let match_keys events =
  let key = Sdl.(Event.(get events keyboard_keycode)) in
  let open Tsdl.Sdl.K in
  if key = up then `Up
  else if key = down then `Down
  else if key = right then `Right
  else if key = left then `Left
  else if key = a then `A
  else if key = s then `B
  else if key = q then `Select
  else if key = w then `Start
  else if key = p then `P
  else `None

let poll_input ({ event; machine; _ } as t) =
  match Sdl.poll_event (Some event) with
  | false -> ()
  | true ->
    match Sdl.Event.(enum (get event typ)) with
    | `Quit -> t.quit <- true; ()
    | `Key_down -> begin
      let key = match_keys event in
      match key with
      | `P -> t.paused <- not t.paused; ()
      | #Joypad.key as key -> begin
        Joypad.button_down machine key;
        Engine.trigger_interrupt machine Interrupts.joypad
      end
      | `None -> ()
    end
    | `Key_up -> begin
      let key = match_keys event in
      match key with
      | #Joypad.key as key -> begin
        Joypad.button_up machine key;
        Engine.trigger_interrupt machine Interrupts.joypad
      end
      | _ -> ()
    end
    | _ -> ()

let refresh ({ win; machine;   _ } as t) =
  poll_input t;
  let fb = machine.gpu.framebuffer in
  let framebuffer = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (160 * 144) in
  for y = 0 to 143 do
    for x = 0 to 159 do
      match fb.(x).(y) with
      | `White -> framebuffer.{(y * 160) + x} <- 0xFFFFFFl
      | `Black -> framebuffer.{(y * 160) + x} <- 0x000000l
      | `Light_gray -> framebuffer.{(y * 160) + x} <- 0xAAAAAAl
      | `Dark_gray -> framebuffer.{(y * 160) + x} <- 0x777777l
    done
  done;
  let aux () =
    let open Rresult.R.Infix in
    (* let ticks = Sdl.get_ticks () in
     * let diff = Int32.(sub ticks last_tick) in
     * if diff < (Int32.div 1000l 59l) then
     *   Sdl.delay Int32.(sub (Int32.div 1000l 59l) diff);
     * t.last_tick <- ticks; *)
    Sdl.get_window_surface win >>= fun surface ->
    Sdl.create_rgb_surface ~w:160 ~h:144 ~depth:32
      0x000000l 0x000000l 0x000000l 0x000000l >>= fun sf ->
    Sdl.lock_surface sf >>= fun () ->
    let pixels = Sdl.get_surface_pixels sf Bigarray.int32 in
    Bigarray.Array1.blit framebuffer pixels;
    Sdl.unlock_surface sf;
    Sdl.blit_scaled ~src:sf None ~dst:surface None >>= fun () ->
    Sdl.update_window_surface win >>= fun () ->
    Sdl.free_surface sf;
    Ok ()
  in
  match aux () with
  | _ -> machine.gpu.redraw <- false

let create machine =
  let audio_setup () =
    (* let audio_callback' output =
     *   let open Bigarray in
     *   Array1.blit machine.Machine.apu.buffer output;
     * in
     * audio_callback := (Some (Sdl.audio_callback Bigarray.char audio_callback')); *)

    let desired_audiospec =
      { Sdl.as_freq = machine.Machine.config.sample_rate;
        as_format = Sdl.Audio.s16_sys;
        Sdl.as_channels = 2;
        Sdl.as_samples = machine.config.sample_size;
        Sdl.as_silence = 0;
        Sdl.as_size = 0l;
        Sdl.as_callback = None; }
    in

    match Sdl.open_audio_device None false desired_audiospec 0 with
    | Error _ -> Sdl.log "Can't open audio device"; exit 1
    | Ok (device_id, _) -> device_id
  in

  match Sdl.init Sdl.Init.everything with
  | Error (`Msg e) -> log_err "Sdl.init: %s" e; assert false
  | Ok () ->
    let flags = Sdl.Window.(shown + opengl) in
    match Sdl.create_window ~w:320 ~h:288 "Goodboy" flags with
    | Error (`Msg e) -> log_err "Sdl.create_window: %s" e; assert false
    | Ok win ->
      let event = Sdl.Event.create () in
      let device_id = audio_setup () in
      let () = Sdl.pause_audio_device device_id false in
      log "SDL started";
      {
        device_id;
        machine;
        win;
        event;
        quit = false;
        step = None;
        last_tick = Sdl.get_ticks ();
        last_poll = Sdl.get_ticks ();
        paused = false;
        breakpoints = Breakpoints.empty;
      }

let step t =
  match t with
  | { quit = true; _ } -> teardown t; Sdl.quit ()
  | { machine; paused = false; _ } ->
    Cpu_exec.step machine;
    if machine.gpu.redraw then
      refresh t;
    if Breakpoints.exists (Int.equal machine.cpu.pc) t.breakpoints then
      t.paused <- true;
    (match t.step with
    | Some step -> begin
      if step > 0 then
        t.step <- Some (step - 1)
      else begin
        t.step <- None;
        t.paused <- true
      end
    end
    | None -> ());
    if machine.apu.need_queue then (
      match Sdl.queue_audio t.device_id machine.apu.buffer with
      | Error _ -> assert false
      | Ok _ ->
        while Sdl.get_queued_audio_size t.device_id > machine.config.sample_size * 4 do
          Sdl.delay Int32.one;
        done;
        machine.apu.need_queue <- false
    )
  | { paused = true; _ } -> poll_input t
