open Astring

type command =
  | Breakpoints
  | Breakpoint of int
  | Search of int
  | Step of int
  | Print of int
  | Resume
  | Quit

exception Parse_error of (int * int)

let parse_error s =
  let st = String.Sub.start_pos s in
  let ed = String.Sub.stop_pos s in
  raise (Parse_error (st, ed))
    
let cmd_of_string s =
  let s = String.Sub.to_string s in
  match String.Ascii.lowercase s with
  | "breakpoints" -> Some `Breakpoints
  | "p"
  | "print" -> Some `Print
  | "step" -> Some `Step
  | "search" -> Some `Search
  | "breakpoint"
  | "b" -> Some `Breakpoint
  | "resume" -> Some `Resume
  | "q"
  | "quit" -> Some `Quit
  | _ -> None

let parse : string -> (command, (int * int)) result =
  fun s -> try
  let skip_white s = String.Sub.drop ~sat:Char.Ascii.is_white s in
  let parse_cmd s =
    let is_command_char c = Char.Ascii.is_letter c || c = '_' in
    match String.Sub.span ~min:1 ~sat:is_command_char s with
    | (cmd, rem) ->
      match cmd_of_string cmd with
      | Some cmd -> cmd, rem
      | None -> parse_error cmd
  in
  let parse_ocaml_int s =
    let is_ocaml_int_char c = Char.Ascii.is_hex_digit c || c = '_' || c = 'x' in
    match String.Sub.span ~min:1 ~sat:is_ocaml_int_char s with
    | (i, rem) ->
      try int_of_string (String.Sub.to_string i), rem with
      | Invalid_argument _ -> parse_error i
  in 
  let (cmd, s) = skip_white (String.sub s) |> parse_cmd in
  match cmd with
  | `Step ->
    let i, _ = skip_white s |> parse_ocaml_int in
    Ok (Step i)
  | `Search ->
    let i, _ = skip_white s |> parse_ocaml_int in
    Ok (Search i)
  | `Breakpoint ->
    let i, _ = skip_white s |> parse_ocaml_int in
    Ok (Breakpoint i)
  | `Resume -> Ok Resume
  | `Print ->
    let i, _ = skip_white s |> parse_ocaml_int in
    Ok (Print i)
  | `Breakpoints -> Ok Breakpoints
  | `Quit -> Ok Quit
 with
 | Parse_error (s, e) -> Error (s, e)   
