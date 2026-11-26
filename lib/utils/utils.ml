open Base
open Stdio
include Intcode

let read_lines ~path =
  In_channel.read_lines path |> List.filter ~f:(fun l -> not (String.is_empty l))
;;
