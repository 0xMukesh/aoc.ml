open Core

let solve ~input : Solution.t =
  let data =
    match Utils.read_lines ~path:input with
    | [] -> Array.of_list []
    | x :: _ -> String.split ~on:',' x |> List.map ~f:Int.of_string |> Array.of_list
  in
  let part_01 =
    match Utils.run_intcode data with
    | Ok result -> Int.to_string result
    | Error err -> sprintf "Error: %s" (Utils.show_intcode_error err)
  in
  { part_01; part_02 = "TODO" }
;;
