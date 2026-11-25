open Printf
open Base

let read_lines ~path : string list =
  In_channel.with_open_text path In_channel.input_lines
  |> List.filter ~f:(fun l -> not (String.is_empty l))
;;
