let solve ~year ~day ~input =
  match year with
  | 2019 -> Solutions_2019.solve ~day ~input
  | _ -> failwith (Printf.sprintf "could not find solutions for year %d" year)
;;
