open Base
open Stdio

let is_monotonic s =
  let rec loop i accum =
    if i = String.length s
    then accum
    else if i = 0
    then loop (i + 1) true
    else (
      let prev = s.[i - 1] in
      let curr = s.[i] in
      loop (i + 1) (accum && Char.compare prev curr <> 1))
  in
  loop 0 true
;;

let count_adjacent_pairs s =
  String.foldi
    s
    ~init:(Map.empty (module Char))
    ~f:(fun i map _ ->
      if i < String.length s - 1 && Char.compare s.[i] s.[i + 1] = 0
      then
        Map.update map s.[i] ~f:(fun o ->
          match o with
          | None -> 1
          | Some d -> d + 1)
      else map)
;;

let count_digits ~low ~high ~checker =
  let rec loop i accum =
    if i > high
    then accum
    else (
      let s = Int.to_string i in
      loop (i + 1) (accum + if checker s then 1 else 0))
  in
  loop low 0
;;

let part_01_checker s =
  let adjacent_pairs = count_adjacent_pairs s in
  let is_monotonic = is_monotonic s in
  is_monotonic && Map.length adjacent_pairs >= 1
;;

let part_02_checker s =
  let adjacent_pairs = count_adjacent_pairs s in
  let is_monotonic = is_monotonic s in
  let doubles_condition =
    Map.fold adjacent_pairs ~init:false ~f:(fun ~key:_ ~data accum -> accum || data = 1)
  in
  is_monotonic && doubles_condition
;;

let solve ~input : Solution.t =
  let parts = Utils.read_lines ~path:input |> List.hd_exn |> String.split ~on:'-' in
  let low, high =
    match parts with
    | [ a; b ] -> Int.of_string a, Int.of_string b
    | _ -> failwith "invalid input syntax"
  in
  let partial = count_digits ~low ~high in
  let part_01 = partial ~checker:part_01_checker |> Int.to_string in
  let part_02 = partial ~checker:part_02_checker |> Int.to_string in
  { part_01; part_02 }
;;
