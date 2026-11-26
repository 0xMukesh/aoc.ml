open Base
open Stdio

type intcode_parameter_mode =
  | Position
  | Immediate
[@@deriving enum, eq]

type double_param_modes = intcode_parameter_mode * intcode_parameter_mode [@@deriving eq]

type intcode_ops =
  | Add of double_param_modes
  | Multiply of double_param_modes
  | Input
  | Output of intcode_parameter_mode
  | JumpIfTrue of double_param_modes
  | JumpIfFalse of double_param_modes
  | LessThan of double_param_modes
  | Equals of double_param_modes
  | Halt
[@@deriving eq]

type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
  | Unknown_param_mode of int
  | Missing_input
[@@deriving show]

let ( let* ) result f = Result.bind result ~f

let parse_op op =
  let base = op % 100 in
  let parse_param_mode mode =
    match intcode_parameter_mode_of_enum mode with
    | Some v -> Ok v
    | None -> Error (Unknown_param_mode mode)
  in
  let parse_single_param op =
    let* param_mode = parse_param_mode (op / 100 % 10) in
    Ok (Output param_mode)
  in
  let parse_double_params op base =
    let* param_one_mode = parse_param_mode (op / 100 % 10) in
    let* param_two_mode = parse_param_mode (op / 1000 % 10) in
    let params = param_one_mode, param_two_mode in
    match base with
    | 1 -> Ok (Add params)
    | 2 -> Ok (Multiply params)
    | 5 -> Ok (JumpIfTrue params)
    | 6 -> Ok (JumpIfFalse params)
    | 7 -> Ok (LessThan params)
    | 8 -> Ok (Equals params)
    | _ -> Error (Unknown_opcode op)
  in
  match base with
  | 1 | 2 | 5 | 6 | 7 | 8 -> parse_double_params op base
  | 3 -> Ok Input
  | 4 -> parse_single_param op
  | 99 -> Ok Halt
  | _ -> Error (Unknown_opcode op)
;;

let run_intcode ?(inputs = []) program =
  let n = Array.length program in
  let inputs = ref inputs in
  let outputs = ref [] in

  let check_pos pos =
    if pos >= 0 && pos < n then Ok () else Error (Memory_out_of_bounds pos)
  in

  let read_param mode offset =
    let* () = check_pos offset in
    let n = program.(offset) in
    if equal_intcode_parameter_mode mode Position then Ok program.(n) else Ok n
  in

  let handle_binary pc param_modes ~f =
    let param_one_mode, param_two_mode = param_modes in
    let* a = read_param param_one_mode (pc + 1) in
    let* b = read_param param_two_mode (pc + 2) in
    let result_pos = program.(pc + 3) in
    let result = f a b in
    program.(result_pos) <- result;
    Ok (pc + 4)
  in

  let handle_input pc =
    let result_pos = program.(pc + 1) in
    match !inputs with
    | [] -> Error Missing_input
    | v :: rest ->
      inputs := rest;
      program.(result_pos) <- v;
      Ok (pc + 2)
  in

  let handle_output pc param_mode =
    let* value = read_param param_mode (pc + 1) in
    outputs := value :: !outputs;
    Ok (pc + 2)
  in

  let handle_jump pc param_modes ~f =
    let param_one_mode, param_two_mode = param_modes in
    let* cond = read_param param_one_mode (pc + 1) in
    let* jump_to = read_param param_two_mode (pc + 2) in
    if f cond then Ok jump_to else Ok (pc + 3)
  in

  let rec loop pc =
    if pc < 0 || pc >= n
    then Error (Memory_out_of_bounds pc)
    else
      let* op = parse_op program.(pc) in
      if equal_intcode_ops op Halt
      then Ok pc
      else
        let* pc =
          match op with
          | Add param_modes -> handle_binary pc param_modes ~f:( + )
          | Multiply param_modes -> handle_binary pc param_modes ~f:( * )
          | Input -> handle_input pc
          | Output param_mode -> handle_output pc param_mode
          | JumpIfTrue param_modes -> handle_jump pc param_modes ~f:(fun x -> x <> 0)
          | JumpIfFalse param_modes -> handle_jump pc param_modes ~f:(fun x -> x = 0)
          | LessThan param_modes ->
            handle_binary pc param_modes ~f:(fun x y -> if x < y then 1 else 0)
          | Equals param_modes ->
            handle_binary pc param_modes ~f:(fun x y -> if x = y then 1 else 0)
          | _ -> assert false
        in
        loop pc
  in
  match loop 0 with
  | Ok _ -> Ok (program.(0), List.rev !outputs)
  | Error message -> Error message
;;
