open Printf

let read_lines filename = In_channel.with_open_text filename In_channel.input_lines
let print_result part_01 part_02 = printf "part 1: %f\npart 2: %f\n" part_01 part_02
