open Betterboy

let halfblock = "▄"

let pxmatrix w h f = Notty.I.tabulate w h @@ fun x y ->
  let y = y * 2 in
  Notty.I.string Notty.A.(bg (f x y) ++ fg (f x (y + 1))) halfblock

let white = Notty.A.white
let dark_gray = Notty.A.(gray 8)
let light_gray = Notty.A.(gray 19)
let black = Notty.A.black

let to_notty_color = function
  | `White -> white
  | `Light_gray -> light_gray
  | `Dark_gray -> dark_gray
  | `Black -> black

let get_tile (m : Machine.t) ?mode:(tile_data=0x8000) tile_number =
  let open Uint8 in
  let tile_number = Uint8.inj tile_number in
  let c1, c2, c3, c4 =
    let open Uint8 in
    (m.gpu.bg_pal) land (inj 0b11),
    (m.gpu.bg_pal lsr Uint8.inj 2) land (inj 0b11),
    (m.gpu.bg_pal lsr Uint8.inj 4) land (inj 0b11),
    (m.gpu.bg_pal lsr Uint8.inj 6) land (inj 0b11)
  in
  let tile_data_ptr =
    if tile_data != 0x9000 then
      (tile_data + (Uint8.proj tile_number) * 0x10)
    else
      (tile_data + (Uint8.to_signed tile_number) * 0x10)
  in
  pxmatrix 8 4 begin fun x y ->
    let tile_data_ptr = tile_data_ptr + (y * 2) in
    let b1 = Mmu.get m tile_data_ptr in
    let b2 = Mmu.get m (tile_data_ptr + 1) in
    let bit = 7 - x mod 8 in
    let low = if is_bit_set b1 bit then 0x01 else 0x00 in
    let high = if is_bit_set b2 bit then 0x02 else 0x00 in
    let col_num = low + high in
    let color = match col_num with
      | 0x0 -> c1
      | 0x1 -> c2
      | 0x2 -> c3
      | 0x3 -> c4
      | _ -> assert false
    in
    let color = Gpu.map_color color in
    to_notty_color color
  end

let get_tiles (m : Machine.t) tiles =
  List.map begin fun l ->
    List.map (fun i -> get_tile m i) l |> Notty.I.hcat
  end tiles
  |> Notty.I.vcat

let tile_addr mode tile =
  let tile = Uint8.inj tile in
  if mode != 0x9000 then
    (mode + (Uint8.proj tile) * 0x10)
  else
    (mode + (Uint8.to_signed tile) * 0x10)
