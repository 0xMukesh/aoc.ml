open Printf
open Base

let read_lines (filename : string) : string list =
  In_channel.with_open_text filename In_channel.input_lines
  |> List.filter ~f:(fun l -> not (String.is_empty l))
;;
