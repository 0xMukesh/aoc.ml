open Base
open Stdio
include Intcode

let read_lines ~path =
  In_channel.read_lines path |> List.filter ~f:(fun l -> not (String.is_empty l))
;;

let read_intcode_input_from_file ~path =
  match read_lines ~path with
  | [] -> Array.of_list []
  | x :: _ -> String.split ~on:',' x |> List.map ~f:Int.of_string |> Array.of_list
;;
