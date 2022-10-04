open Common
open Betterboy
open Nottui
open Nottui_widgets

module Pp = Pretty_opcodes

let cur = Lwd.var 0x0
let pc = Lwd.var 0x0

let bp_handler opcode ui =
  let mouse_handler = fun ~x:_ ~y:_ m ->
    match m with
    | `Left ->
      let s = Lwd.peek breakpoints in
      let b =
        if Breakpoints.exists (Int.equal opcode) s then
          Breakpoints.remove opcode s
        else
          Breakpoints.add opcode s
      in
      Lwd.set breakpoints b;
      `Handled
    | _ -> `Unhandled
  in
  Ui.mouse_area mouse_handler ui

exception Done

let headers = [
  "PC/BP";
  "REGION";
  "ADDR";
  "VAL";
  "OP";
  "SIZE";
  "OP1";
  "OP2";
  "IMM";
  "IMM";
  "NAME"
] |> List.map Nottui_widgets.string |> List.map Lwd.pure

let disassemble (m : Machine.t) symfile =

  let check_symfile addr =
    match symfile with
    | None -> printf ""
    | Some s ->
    match Cartridge.rom_bank m.cartridge with
    | None -> printf ""
    | Some bank ->
    match Hashtbl.find_opt s ((if addr > 0x4000 then bank else 0), addr) with
    | None -> printf ""
    | Some name -> printf "%s" name
  in

  let go cur breakpoints =

    let i = ref cur in
    let count = ref 80 in

    let append (l : 'a list ref) (e : 'a) : unit = l := List.append !l [e] in

    let rows = ref [] in

    let skip_until o =
      let rec aux i =
        let sz =
          match Pp.get_opcode_repr o with
          | exception Not_found -> 1
          | { size; _ } -> size
        in
        if (i + sz) >= o then
          i
        else
          aux (i + sz)
      in
      aux (if m.cpu.pc < o then m.cpu.pc else 0x0)
    in

    let compile is_extended o =
      let compiled =
        try
          Some (if is_extended then Pp.get_ext_opcode_repr o else Pp.get_opcode_repr o)
        with
        | _ -> None
      in
      let state =
        if m.cpu.pc = !i then
          printf ~attr:Notty.A.(fg green) "▶"
        else if Breakpoints.exists (Int.equal !i) breakpoints then
          printf ~attr:Notty.A.(fg red) "⬤"
        else
          printf "  "
      in
      let region = printf "%s" ~attr:Notty.A.(fg (gray 10)) (Regions.regions m !i) in
      let sym =
        if Regions.is_between' !i Machine.Cartridge.range then
          check_symfile !i
        else
          printf ""
      in
      let addr = printf "%04X" !i in
      let values =
        if is_extended then
          printf ~attr:Notty.A.(fg blue) "CB %02X" o
        else
          printf ~attr:Notty.A.(fg blue) "   %02X" o
      in
      match compiled with
      | None ->
        let pp = printf "" in
        let sz = printf "" in
        let op1 = printf "" in
        let op2 = printf "" in
        let imm_n = printf "" in
        let imm_nn = printf "" in
        let row =
          List.map (bp_handler !i) [state;region;addr;values;pp;sz;op1;op2;imm_n;imm_nn;sym]
          |> List.map Lwd.pure
        in
        i := !i + 1;
        count := !count - 1;
        append rows row
      | Some compiled ->
        let pp = printf ~attr:Notty.A.(fg green) "%s" compiled.mnemo in
        let sz = printf ~attr:Notty.A.(fg (gray 10)) "%d" compiled.size in
        let op1 =
          match compiled.operand1 with
         | Some op -> printf ~attr:Notty.A.(fg red) "%s" op
         | None -> printf ""
        in
        let op2 =
          match compiled.operand2 with
         | Some op -> printf ~attr:Notty.A.(fg red) "%s" op
         | None -> printf ""
        in
        let (imm_n, imm_nn) =
          match compiled.size with
          | 1 -> printf "", printf ""
          | 2 ->
           let n = Mmu.get m (!i + 1) |> Uint8.proj in
           printf ~attr:Notty.A.(fg yellow) "%02X" n,
           printf ""
         | 3 ->
           let n = Mmu.get m (!i + 1) |> Uint8.proj in
           let nn = Mmu.get m (!i + 2) |> Uint8.proj in
           printf ~attr:Notty.A.(fg yellow) "%02X" n,
           printf ~attr:Notty.A.(fg yellow) "%02X" nn
         | _ -> assert false
        in

        let row =
          List.map (bp_handler !i) [state;region;addr;values;pp;sz;op1;op2;imm_n;imm_nn;sym]
          |> List.map Lwd.pure
        in

        i := if is_extended then !i + compiled.size - 1 else !i + compiled.size;
        count := !count - 1;
        append rows row
  in

  let c = skip_until !i in
  i := c;
  begin try
    while true do

      let opcode = Mmu.get m !i in

      (match Uint8.proj opcode with
      | 0xCB -> i:= !i + 1; compile true (Mmu.get m !i |> Uint8.proj)
      | opcode -> compile false opcode);

      if !count = 0 then
        raise Done;

    done
    with
    | Done -> ()
  end;
  Nottui_widgets.grid ~headers ~h_space:2 !rows
  in
  let focus_handler = function
    | `Arrow `Down, []   -> Lwd.set cur (Lwd.peek cur + 1); `Handled
    | `Arrow `Up , []  -> Lwd.set cur (Lwd.peek cur - 1); `Handled
    | `Page `Down, []   -> Lwd.set cur (Lwd.peek cur + 15); `Handled
    | `Page `Up , []  -> Lwd.set cur (Lwd.peek cur - 15); `Handled
    | _ -> `Unhandled
  in
  let scroll_handler ~x:_ ~y:_ = function
    | `Scroll `Down   -> Lwd.set cur (Lwd.peek cur + 1); `Handled
    | `Scroll `Up   -> Lwd.set cur (Lwd.peek cur - 1); `Handled
    | _ -> `Unhandled
  in
  Lwd.map2 (Lwd.get cur) (Lwd.get breakpoints) ~f:(fun cur breakpoints ->
  let open Lwd.Infix in
  go cur breakpoints
  >>= fun ui ->
  Ui.mouse_area scroll_handler ui
  |> Ui.keyboard_area focus_handler
  |> Lwd.return)
