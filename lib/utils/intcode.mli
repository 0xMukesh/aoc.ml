type intcode_error =
  | Memory_out_of_bounds of int
  | Unknown_opcode of int
  | Unknown_param_mode of int
  | Missing_input
[@@deriving show]

val run_intcode : ?inputs:int list -> int array -> (int * int list, intcode_error) result
