open Base
open Stdio

type dir =
  | Up
  | Left
  | Down
  | Right
[@@deriving enum]

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

module Wire = struct
  type t =
    { pos : int * int
    ; step_count : int
    }

  let make () = { pos = 0, 0; step_count = 0 }

  let step t dir : t =
    let new_pos =
      let x, y = t.pos in
      match dir with
      | Up -> x, y - 1
      | Left -> x - 1, y
      | Down -> x, y + 1
      | Right -> x + 1, y
    in
    { pos = new_pos; step_count = t.step_count + 1 }
  ;;
end

let transform_move_str s =
  let step_size = String.sub s ~pos:1 ~len:(String.length s - 1) |> Int.of_string in
  let dir =
    match String.get s 0 with
    | 'U' -> Up
    | 'L' -> Left
    | 'D' -> Down
    | 'R' -> Right
    | c -> failwith (Printf.sprintf "unknown direction prefix: %c" c)
  in
  dir, step_size
;;

let advance_first_wire path table =
  List.fold path ~init:(Wire.make ()) ~f:(fun wire (dir, steps) ->
    let rec loop wire i =
      if i = 0
      then wire
      else (
        let wire = Wire.step wire dir in
        Hashtbl.set table ~key:wire.pos ~data:wire.step_count;
        loop wire (i - 1))
    in
    loop wire steps)
;;

let part_01_solver (x, y) best ~table:_ ~step_count:_ =
  Int.min best (Int.abs x + Int.abs y)
;;

let part_02_solver (x, y) best ~table ~step_count =
  let first_wire_step_count =
    Hashtbl.find table (x, y) |> Option.value_or_thunk ~default:(fun () -> 0)
  in
  Int.min best (first_wire_step_count + step_count)
;;

let advance_second_wire path table ~solver =
  let _, minimum =
    List.fold
      path
      ~init:(Wire.make (), Int.max_value)
      ~f:(fun (wire, best) (dir, steps) ->
        let rec loop wire i best =
          if i = 0
          then wire, best
          else (
            let wire = Wire.step wire dir in
            let best =
              if Coord.compare wire.pos (0, 0) <> 0 && Hashtbl.mem table wire.pos
              then solver wire.pos best ~table ~step_count:wire.step_count
              else best
            in
            loop wire (i - 1) best)
        in
        loop wire steps best)
  in
  minimum
;;

let solve ~input =
  let data =
    Utils.read_lines ~path:input
    |> List.map ~f:(fun x -> String.split x ~on:',')
    |> List.map ~f:(fun l -> List.map l ~f:transform_move_str)
  in
  let first_path, second_path =
    match data with
    | [] -> failwith "missing path data for both wires"
    | [ hd ] -> failwith "missing path data for second wire"
    | [ hd; tl ] -> hd, tl
    | _ -> failwith "input consists of path data for more than two wires"
  in
  let table = Hashtbl.create (module Coord) in
  let _ = advance_first_wire first_path table in
  let part_01_result = advance_second_wire second_path table ~solver:part_01_solver in
  let part_02_result = advance_second_wire second_path table ~solver:part_02_solver in
  printf "%d %d\n" part_01_result part_02_result
;;
