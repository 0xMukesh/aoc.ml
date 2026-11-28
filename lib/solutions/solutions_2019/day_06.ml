open Base
open Core

type map = (string, string list, String.comparator_witness) Map.t

let split_once s ~on =
  match String.split s ~on with
  | [ a; b ] -> a, b
  | _ ->
    failwith @@ sprintf "expected each line to only have a single instance of '%c'" on
;;

let build_map (data : (string * string) list) =
  List.fold
    data
    ~init:(Map.empty (module String))
    ~f:(fun map (key, value) ->
      Map.update map key ~f:(fun o ->
        match o with
        | None -> [ value ]
        | Some v -> value :: v))
;;

let total_routes map =
  Map.fold map ~init:0 ~f:(fun ~key ~data total ->
    let rec loop key accum =
      match Map.find map key with
      | None -> accum
      | Some children ->
        List.fold children ~init:accum ~f:(fun acc child -> loop child (acc + 1))
    in
    total + loop key 0)
;;

let dist_from_root (map : map) node =
  let rec loop key accum =
    match Map.find map key with
    | None -> if String.equal key node then Some accum else None
    | Some children ->
      List.find_map children ~f:(fun child -> loop child (child :: accum))
  in
  loop "COM" []
;;

let find_first_common_ancestor a b =
  let a, b = List.rev a, List.rev b in
  let smaller, larger = if List.length a < List.length b then a, b else b, a in
  let rec loop smaller larger (prev, i) =
    match smaller with
    | [] -> None
    | hd :: tl ->
      if String.equal hd (List.hd_exn larger)
      then loop tl (List.tl_exn larger) (hd, i + 1)
      else Some i
  in
  loop smaller larger ("", 0)
;;

let solve ~input : Solution.t =
  let data =
    Utils.read_lines ~path:input |> List.map ~f:(fun s -> split_once s ~on:')')
  in
  let map = build_map data in
  let part_01 = total_routes map |> Int.to_string in
  let you_path =
    dist_from_root map "YOU" |> Option.value_exn ~message:"failed to find YOU object"
  in
  let san_path =
    dist_from_root map "SAN" |> Option.value_exn ~message:"failed to find SAN object"
  in
  let index =
    find_first_common_ancestor you_path san_path
    |> Option.value_exn ~message:"failed to find first common ancestor"
  in
  let part_02 =
    List.length you_path - index - 1 + (List.length san_path - index - 1) |> Int.to_string
  in
  { part_01; part_02 }
;;
