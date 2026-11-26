open Base
open Stdio

type intcode_parameter_mode =
  | Position
  | Immediate
[@@deriving enum, eq]

type intcode_ops =
  | Add of (intcode_parameter_mode * intcode_parameter_mode)
  | Multiply of (intcode_parameter_mode * intcode_parameter_mode)
  | Input
  | Output of intcode_parameter_mode
  | Halt
[@@deriving eq]

type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
  | Unknown_param_mode of int
[@@deriving show]

let ( let* ) result f = Result.bind result ~f

let parse_op op =
  let base = op % 100 in
  let parse_param_mode mode =
    match intcode_parameter_mode_of_enum mode with
    | Some v -> Ok v
    | None -> Error (Unknown_param_mode mode)
  in
  let parse_binary op base =
    let* param_one_mode = parse_param_mode (op / 100 % 10) in
    let* param_two_mode = parse_param_mode (op / 1000 % 10) in
    let params = param_one_mode, param_two_mode in
    if base = 1 then Ok (Add params) else Ok (Multiply params)
  in
  let parse_output op =
    let* param_mode = parse_param_mode (op / 100 % 10) in
    Ok (Output param_mode)
  in
  match base with
  | 1 | 2 -> parse_binary op base
  | 3 -> Ok Input
  | 4 -> parse_output op
  | 99 -> Ok Halt
  | _ -> Error (Unknown_opcode base)
;;

let run_intcode input =
  let n = Array.length input in

  let check_pos pos =
    if pos >= 0 && pos < n then Ok () else Error (Memory_out_of_bounds pos)
  in

  let read_param mode offset =
    let* () = check_pos offset in
    let n = input.(offset) in
    if equal_intcode_parameter_mode mode Position then Ok input.(n) else Ok n
  in

  let handle_arithmetic pc param_modes operator =
    let param_one_mode, param_two_mode = param_modes in
    let* a = read_param param_one_mode (pc + 1) in
    let* b = read_param param_two_mode (pc + 2) in
    let result_pos = input.(pc + 3) in
    let result = operator a b in
    input.(result_pos) <- result;
    Ok 4
  in

  let handle_input pc =
    let result_pos = input.(pc + 1) in
    let result = In_channel.input_line_exn In_channel.stdin |> Int.of_string in
    input.(result_pos) <- result;
    Ok 2
  in

  let handle_output pc param_mode =
    let* value = read_param param_mode (pc + 1) in
    Out_channel.output_line Out_channel.stdout (Int.to_string value);
    Ok 2
  in

  let rec loop pc =
    if pc < 0 || pc >= n
    then Error (Memory_out_of_bounds pc)
    else
      let* op = parse_op input.(pc) in
      if equal_intcode_ops op Halt
      then Ok pc
      else
        let* to_increment =
          match op with
          | Add param_modes -> handle_arithmetic pc param_modes ( + )
          | Multiply param_modes -> handle_arithmetic pc param_modes ( * )
          | Input -> handle_input pc
          | Output param_mode -> handle_output pc param_mode
          | _ -> assert false
        in
        loop (pc + to_increment)
  in
  match loop 0 with
  | Ok _ -> Ok input.(0)
  | Error message -> Error message
;;
