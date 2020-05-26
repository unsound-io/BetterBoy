open Nottui
open Nottui_widgets

let vram () =
  let current_tile = Lwd.var 0x0 in
  let tiles = Commands.make_range_matrix 0x00 0xFF in
  Lwd.map2' (Lwd.get current_tile) (Lwd.get Common.machine) @@ fun tile m ->
  let vram =
    List.map begin fun l ->
      List.map begin fun i ->
        let tile = Tiles.get_tile m i |> Nottui.Ui.atom in
        let mouse_handler ~x:_ ~y:_ = function
        | `Left   -> Lwd.set current_tile i; `Handled
        | _ -> `Unhandled
        in
        Nottui.Ui.mouse_area mouse_handler tile
        |> Lwd.pure
      end l
    end tiles
    |> Nottui_widgets.grid
  in
  let current =
    let t = Tiles.get_tile m tile |> Ui.atom in
    let addr = printf "Tile address: %04X" (Tiles.tile_addr tile) in
    let num = printf "Tile number: %02X" tile in
    Ui.vcat [t; Ui.void 1 1; addr; Ui.void 1 0; num;]
    |> Lwd.pure
  in
  Lwd_utils.pack Ui.pack_x [vram; Ui.void 1 1 |> Lwd.pure; current]
