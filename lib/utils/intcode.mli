type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
  | Unknown_param_mode of int
[@@deriving show]

val run_intcode : int array -> (int, intcode_error) result
