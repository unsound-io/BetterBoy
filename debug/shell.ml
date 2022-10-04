open Common
open Nottui
open Notty
open Betterboy

let backlog = Lwd_table.make ()

let cpu_state (machine : Machine.t) =
  let open Nottui_widgets in
  let checkmark = "✔" in
  let cross = "✘" in
  let bool b = if b then checkmark else cross in
  let cpu = machine.cpu in
  let b = printf ~attr:Notty.A.(fg cyan ++ st bold) "B:%02X " (Uint8.proj cpu.b) in
  let c = printf ~attr:Notty.A.(fg cyan ++ st bold) "C:%02X " (Uint8.proj cpu.c) in
  let d = printf ~attr:Notty.A.(fg cyan ++ st bold) "D:%02X " (Uint8.proj cpu.d) in
  let e = printf ~attr:Notty.A.(fg cyan ++ st bold) "E:%02X " (Uint8.proj cpu.e) in
  let h = printf ~attr:Notty.A.(fg cyan ++ st bold) "H:%02X " (Uint8.proj cpu.h) in
  let l = printf ~attr:Notty.A.(fg cyan ++ st bold) "L:%02X " (Uint8.proj cpu.l) in
  let a = printf ~attr:Notty.A.(fg cyan ++ st bold) "A:%02X " (Uint8.proj cpu.a) in
  let f = printf ~attr:Notty.A.(fg cyan ++ st bold) "F:%02X " (Cpu.get_register machine Cpu.F |> Uint8.proj) in
  let z = printf ~attr:Notty.A.(fg yellow ++ st bold)"Z:%s " (bool cpu.z) in
  let n = printf ~attr:Notty.A.(fg yellow ++ st bold) "N:%s " (bool cpu.n) in
  let hc = printf ~attr:Notty.A.(fg yellow ++ st bold) "HC:%s " (bool cpu.hc) in
  let ca = printf ~attr:Notty.A.(fg yellow ++ st bold) "CA:%s " (bool cpu.ca) in
  let pc = printf ~attr:Notty.A.(fg red ++ st bold) "PC:%04X " cpu.pc in
  let sp = printf ~attr:Notty.A.(fg red ++ st bold) "SP:%04X " cpu.sp in
  let if_ = printf ~attr:Notty.A.(fg green ++ st bold) "IF:%02X " (Mmu.get machine 0xFF0F |> Uint8.proj) in
  let ie= printf ~attr:Notty.A.(fg green ++ st bold) "IE:%02X " (Mmu.get machine 0xFFFF |> Uint8.proj) in
  let ime = printf ~attr:Notty.A.(fg green ++ st bold) "IME:%s " (bool cpu.ime) in
  [a;f;b;c;d;e;h;l;pc;sp;z;n;hc;ca;if_;ie;ime]
  |> Ui.hcat


let sub' str p l =
  if p = 0 && l = String.length str
  then str
  else String.sub str p l

let delete_word s pos =
  let open Astring in
  if pos = 0 then
    (s, pos)
  else
  let tl = String.sub ~start:pos s in
  let hd = String.sub ~start:0 ~stop:pos s in
  let hd', _ =
    match String.Sub.get hd (String.Sub.length hd - 1) with
    | c when Char.Ascii.is_white c ->
      String.Sub.span ~rev:true ~sat:Char.Ascii.is_white hd
    | _ ->
      String.Sub.span ~rev:true ~sat:(fun c -> not (Char.Ascii.is_white c)) hd
  in
  let pos = String.Sub.length hd' in
  String.Sub.append hd' tl |> String.Sub.base_string, pos

let add_command_to_backlog backlog s =
  let c = Nottui_widgets.printf "> %s" s in
  Lwd_table.append' backlog (Lwd.pure c)

let execute_command backlog s cmd =
  let open Nottui_widgets in
  add_command_to_backlog backlog s;
  match cmd with
  | Commands.Search i -> Lwd.set Disassembler.cur i
  | Commands.Breakpoint bp ->
    let s = Lwd.peek breakpoints in
    let b =
      if Breakpoints.exists (Int.equal bp) s then
        Breakpoints.remove bp s
      else
        Breakpoints.add bp s
    in
    Lwd.set breakpoints b
  | Breakpoints ->
    let b = Lwd.peek breakpoints in
    Breakpoints.iter
      (fun i -> Lwd_table.append' backlog (printf "%04X" i |> Lwd.pure)) b
  | Tile i ->
    let tile = Tiles.get_tile (Lwd.peek machine) i in
    Lwd_table.append' backlog (Ui.atom tile |> Lwd.pure)
  | Tiles range ->
    let tiles = Tiles.get_tiles (Lwd.peek machine) range in
    Lwd_table.append' backlog (Ui.atom tiles |> Lwd.pure)
  | Step i -> raise (Step i)
  | Resume -> raise Resume
  | Quit -> raise Quit
  | Print i ->
    let s =
      try
        printf "%02x" (Betterboy.Mmu.get (Lwd.peek Common.machine) i |> Uint8.proj)
      with
      | _ -> printf "nil"
    in
    Lwd_table.append' backlog (Lwd.pure s)

let update_field focus (text, pos) =
  Ui.atom @@ I.hcat @@
  if Focus.has_focus focus then (
    let attr = A.(bg lightblue) in
    let len = String.length text in
    (if pos >= len
     then [I.string attr text]
     else [I.string attr (sub' text 0 pos)])
    @
    (if pos < String.length text then
       [I.string A.(bg lightred) (sub' text pos 1);
        I.string attr (sub' text (pos + 1) (len - pos - 1))]
     else [I.string A.(bg lightred) " "]);
  ) else
    [I.string A.(st underline) (if text = "" then " " else text)]

let shell_field state ~on_change =
  let focus = Focus.make() in
  let update focus_h focus (text, pos, hist) =
    let pos = min (max 0 pos) (String.length text) in
    let content = update_field focus (text, pos) in

    let on_submit (s, _, hist) =
      match Commands.parse s with
      | Ok cmd ->
        on_change ("", 0, s::hist);
        execute_command backlog s cmd
      | _ -> ()
    in

    let handler = function
      | `Arrow `Up, _ -> begin
        match List.hd hist with
        | exception _ -> on_change (text, pos, hist); `Handled
        | cmd -> on_change (cmd, String.length cmd, hist); `Handled
      end
      | `ASCII 'L', [`Ctrl] -> Lwd_table.clear backlog; `Handled
      | `ASCII 'U', [`Ctrl] -> on_change ("", 0, hist); `Handled
      | `ASCII 'W', [`Ctrl] -> begin
        let (s, pos) = delete_word text pos in
        on_change (s, pos, hist); `Handled
      end
      | `Escape, [] -> Focus.release focus_h; `Handled
      | `ASCII k, _ ->
        let text =
          if pos < String.length text then (
            String.sub text 0 pos ^ String.make 1 k ^
            String.sub text pos (String.length text - pos)
          ) else (
            text ^ String.make 1 k
          )
        in
        on_change (text, (pos + 1), hist);
        `Handled
      | `Backspace, _ ->
        let text =
          if pos > 0 then (
            if pos < String.length text then (
              String.sub text 0 (pos - 1) ^
              String.sub text pos (String.length text - pos)
            ) else if String.length text > 0 then (
              String.sub text 0 (String.length text - 1)
            ) else text
          ) else text
        in
        let pos = max 0 (pos - 1) in
        on_change (text, pos, hist);
        `Handled
      | `Enter, _ -> on_submit (text, pos, hist); `Handled
      | `Arrow `Left, [] ->
        let pos = min (String.length text) pos in
        if pos > 0 then (
          on_change (text, pos - 1, hist);
          `Handled
        )
        else `Unhandled
      | `Arrow `Right, [] ->
        let pos = pos + 1 in
        if pos <= String.length text
        then (on_change (text, pos, hist); `Handled)
        else `Unhandled
      | _ -> `Unhandled
    in
    Ui.keyboard_area ~focus handler content
  in

  let node =
    Lwd.map2 (Focus.status focus) state ~f:(update focus) 
  in

  let mouse_grab (text, pos, h) ~x ~y:_ = function
    | `Left ->
      if x <> pos then on_change (text, x, h);
      Nottui.Focus.request focus;
      `Handled
    | _ -> `Unhandled
  in
  let l = (
    Lwd.join @@
    Lwd_table.reduce (Lwd_utils.lift_monoid Ui.pack_y) backlog
  )
  in
  [
    l;
    (Lwd.map2 state node ~f:(fun state content -> Ui.mouse_area (mouse_grab state) content))
  ]

let prompt : (string * int * string list) Lwd.var = Lwd.var ("", 0, [])

let make () =
  Lwd_table.append' backlog (cpu_state (Lwd.peek Common.machine) |> Lwd.pure);
  shell_field (Lwd.get prompt) ~on_change:(Lwd.set prompt)
