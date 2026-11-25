open Base
open Stdio
open Core

type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
[@@deriving show]

type search_error =
  | Search_out_of_bounds of int
  | No_pair_found
[@@deriving show]

let run_intcode input ~use_1202 =
  if use_1202
  then (
    input.(1) <- 12;
    input.(2) <- 2);

  let n = Array.length input in
  let rec loop pc =
    if pc < 0 || pc >= n
    then Error (Memory_out_of_bounds pc)
    else
      begin match input.(pc) with
      | 99 -> Ok ()
      | (1 | 2) as op ->
        let a_pos = input.(pc + 1) in
        let b_pos = input.(pc + 2) in
        let output_pos = input.(pc + 3) in
        let is_in_bounds pos = pos >= 0 && pos < n in

        if not (is_in_bounds a_pos && is_in_bounds b_pos && is_in_bounds output_pos)
        then Error (Memory_out_of_bounds output_pos)
        else begin
          let a = input.(a_pos) in
          let b = input.(b_pos) in

          input.(output_pos) <- (if op = 1 then a + b else a * b);
          loop (pc + 4)
        end
      | opcode -> Error (Unknown_opcode opcode)
      end
  in

  match loop 0 with
  | Ok () -> Ok input.(0)
  | Error message -> Error message
;;

let find_suitable_pairs input ~desired_result =
  let original_state = Array.copy input in
  let rec search noun verb =
    if noun > 99
    then Error No_pair_found
    else if verb > 99
    then search (noun + 1) 0
    else begin
      let trial_input = Array.copy original_state in
      trial_input.(1) <- noun;
      trial_input.(2) <- verb;

      match run_intcode trial_input ~use_1202:false with
      | Ok result when result = desired_result -> Ok (noun, verb)
      | Ok _ -> search noun (verb + 1)
      | Error (Memory_out_of_bounds position) -> Error (Search_out_of_bounds position)
      | Error (Unknown_opcode _) -> search noun (verb + 1)
    end
  in
  search 0 0
;;

let solve ~input : Solution.t =
  let data =
    match Utils.read_lines ~path:input with
    | [] -> Array.of_list []
    | x :: _ -> String.split ~on:',' x |> List.map ~f:Int.of_string |> Array.of_list
  in
  let part_01 =
    match run_intcode (Array.copy data) ~use_1202:true with
    | Ok result -> Int.to_string result
    | Error err -> sprintf "Error: %s" (show_intcode_error err)
  in
  let part_02 =
    match find_suitable_pairs data ~desired_result:19690720 with
    | Ok (noun, verb) -> (100 * noun) + verb |> Int.to_string
    | Error err -> sprintf "Error: %s" (show_search_error err)
  in
  { part_01; part_02 }
;;
