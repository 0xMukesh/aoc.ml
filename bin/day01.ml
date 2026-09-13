open Base
open Stdio
open Utils

let data = List.map (Utils.read_lines "./_data/2019/input.txt") ~f:Int.of_string

let process_masses (data : int list) ~(f : int -> int) : float =
  Float.of_int (List.fold data ~init:0 ~f:(fun acc x -> acc + f x))
;;

let fuel_requirement_for_mass mass = Int.max ((mass / 3) - 2) 0

let rec total_fuel_requirement mass ~accum =
  match mass with
  | 0 -> accum
  | n when n < 0 -> accum
  | n when n > 0 ->
    let fuel_needed = fuel_requirement_for_mass n in
    total_fuel_requirement fuel_needed ~accum:(accum + fuel_needed)
  | _ -> accum
;;

let () =
  let part_01 = process_masses data ~f:fuel_requirement_for_mass in
  let part_02 = process_masses data ~f:(total_fuel_requirement ~accum:0) in
  print_result part_01 part_02
;;
