open Base
open Stdio
open Core

type search_error =
  | Search_out_of_bounds of int
  | No_pair_found
[@@deriving show]

let run_modified_intcode input =
  input.(1) <- 12;
  input.(2) <- 2;

  Utils.run_intcode input
;;

let find_suitable_pairs input ~desired_result =
  let original_state = Array.copy input in
  let rec search noun verb =
    if noun > 99
    then Error No_pair_found
    else if verb > 99
    then search (noun + 1) 0
    else (
      let trial_input = Array.copy original_state in
      trial_input.(1) <- noun;
      trial_input.(2) <- verb;

      match Utils.run_intcode trial_input with
      | Ok (result, _) when result = desired_result -> Ok (noun, verb)
      | Ok _ -> search noun (verb + 1)
      | Error (Memory_out_of_bounds position) -> Error (Search_out_of_bounds position)
      | Error (Unknown_opcode _) -> search noun (verb + 1)
      | _ -> assert false)
  in
  search 0 0
;;

let solve ~input : Solution.t =
  let data = Utils.read_intcode_input_from_file ~path:input in
  let part_01 =
    match run_modified_intcode (Array.copy data) with
    | Ok (result, _) -> Int.to_string result
    | Error err -> sprintf "Error: %s" (Utils.show_intcode_error err)
  in
  let part_02 =
    match find_suitable_pairs data ~desired_result:19690720 with
    | Ok (noun, verb) -> (100 * noun) + verb |> Int.to_string
    | Error err -> sprintf "Error: %s" (show_search_error err)
  in
  { part_01; part_02 }
;;
