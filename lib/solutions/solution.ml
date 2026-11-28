open Stdio

type t =
  { part_01 : string
  ; part_02 : string
  }

let make ~(f : 'a -> string) ~part_01 ~part_02 : t =
  { part_01 = f part_01; part_02 = f part_02 }
;;

let show t = printf "Part 01: %s\nPart 02: %s\n" t.part_01 t.part_02
