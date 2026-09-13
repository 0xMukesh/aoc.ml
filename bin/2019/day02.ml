open Base
open Stdio
open Result

let filename = "./_data/2019/02/input.txt"

let data =
  match Utils.read_lines filename with
  | [] -> Array.of_list []
  | x :: _ -> String.split ~on:',' x |> List.map ~f:Int.of_string |> Array.of_list
;;

type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
[@@deriving show]

type search_error =
  | Search_out_of_bounds of int
  | No_pair_found
[@@deriving show]

let run_intcode (input : int array) (use_1202 : bool) : (int, intcode_error) Result.t =
  if use_1202
  then begin
    input.(1) <- 12;
    input.(2) <- 2
  end;

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
        let is_in_bounds = Utils.is_in_bounds ~length:n in

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

let find_suitable_pairs (input : int array) (desired_result : int)
  : (int * int, search_error) Result.t
  =
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

      match run_intcode trial_input false with
      | Ok result when result = desired_result -> Ok (noun, verb)
      | Ok _ -> search noun (verb + 1)
      | Error (Memory_out_of_bounds position) -> Error (Search_out_of_bounds position)
      | Error (Unknown_opcode _) -> search noun (verb + 1)
    end
  in
  search 0 0
;;

let () =
  begin match run_intcode (Array.copy data) true with
  | Ok result -> printf "%d\n" result
  | Error err -> printf "%s\n" (show_intcode_error err)
  end;

  let search_desired_result = 19690720 in
  match find_suitable_pairs (Array.copy data) search_desired_result with
  | Ok (noun, verb) -> printf "%d\n" ((100 * noun) + verb)
  | Error err -> printf "%s\n" (show_search_error err)
;;
