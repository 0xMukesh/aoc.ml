open Stdio

let solve ~year ~day ~input =
  let result =
    match year with
    | 2019 -> Solutions_2019.solve ~day ~input
    | _ -> failwith (Printf.sprintf "could not find solutions for year %d" year)
  in
  printf "Part 1: %s\nPart 2: %s\n" result.part_01 result.part_02
;;
