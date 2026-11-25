let solve ~day ~input =
  match day with
  | 1 -> Day01.solve ~input
  | 2 -> Day02.solve ~input
  | _ -> failwith (Printf.sprintf "could not find solution for day %d" day)
;;
