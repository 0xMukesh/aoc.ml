let solve ~day ~input =
  match day with
  | 1 -> Day_01.solve ~input
  | 2 -> Day_02.solve ~input
  | 3 -> Day_03.solve ~input
  | 4 -> Day_04.solve ~input
  | 5 -> Day_05.solve ~input
  | 6 -> Day_06.solve ~input
  | _ -> failwith (Printf.sprintf "could not find solution for day %d" day)
;;
