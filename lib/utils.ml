open Printf
open Base

let read_lines (filename : string) : string list =
  List.filter (In_channel.with_open_text filename In_channel.input_lines) ~f:(fun l ->
    not (String.is_empty l))
;;

let is_in_bounds ~(length : int) (pos : int) : bool = pos >= 0 && pos < length
