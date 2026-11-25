let command =
  Command.basic
    ~summary:"AoC solution runner"
    (let open Command.Let_syntax in
     let%map_open year = flag "--year" (required int) ~doc:"YEAR year of the problem"
     and day = flag "--day" (required int) ~doc:"DAY day of the problem"
     and input = flag "--input" (required string) ~doc:"FILE path of the input file" in
     fun () -> Solutions.solve ~year ~day ~input)
;;

let () = Command_unix.run command
