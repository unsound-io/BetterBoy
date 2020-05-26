open Nottui
open Nottui_widgets

let vram () =
  let current_tile = Lwd.var (0x0, 0x8000) in
  let vram =
    Lwd.map' (Lwd.get Common.machine) @@ fun m ->
    let go mode tiles =
      List.map begin fun l ->
        List.map begin fun i ->
          let tile = Tiles.get_tile ~mode m i |> Nottui.Ui.atom in
          let mouse_handler ~x:_ ~y:_ = function
            | `Left   -> Lwd.set current_tile (i, mode); `Handled
            | _ -> `Unhandled
          in
          Nottui.Ui.mouse_area mouse_handler tile
          |> Lwd.pure
        end l
      end tiles
    in
    let t1 = go 0x8000 (Commands.make_range_matrix 0x00 0xFF) in
    let t2 = go 0x9000 (Commands.make_range_matrix 0x00 0x7F) in
    List.concat [t1; t2]
    |> Nottui_widgets.grid
    |> Nottui_widgets.scroll_area
  in
  let current =
    Lwd.map2' (Lwd.get current_tile) (Lwd.get Common.machine) @@ fun (tile, mode) m ->
    let t = Tiles.get_tile ~mode m tile |> Ui.atom in
    let addr = printf "Tile address: %04X" (Tiles.tile_addr mode tile) in
    let num = printf "Tile number: %02X" tile in
    Ui.vcat [t; Ui.void 1 1; addr; Ui.void 1 0; num;]
  in
  Lwd_utils.pack Ui.pack_x [Lwd.join vram; Ui.void 1 1 |> Lwd.pure; current]
