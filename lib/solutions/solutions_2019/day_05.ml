open Core

let solve ~input =
  let data = Utils.read_intcode_input_from_file ~path:input in
  let execute_program data ~inputs =
    match Utils.run_intcode data ~inputs with
    | Ok (_, outputs) -> outputs
    | Error err -> failwith @@ sprintf "Error: %s" (Utils.show_intcode_error err)
  in
  let part_01 = execute_program (Array.copy data) ~inputs:[ 1 ] in
  let part_02 = execute_program (Array.copy data) ~inputs:[ 5 ] in
  let formatter outputs =
    outputs |> List.map ~f:Int.to_string |> String.concat ~sep:"\n"
  in
  Solution.make ~f:formatter ~part_01 ~part_02
;;
