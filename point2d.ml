open Base
open Printf

type t =
  { x : float
  ; y : float
  }

let create (x, y) = { x; y }
let x p = p.x
let y p = p.y
let to_string p = sprintf "(%f, %f)" p.x p.y
let magnitude p = Float.hypot p.x p.y
let distance p1 p2 = Float.hypot (p2.x -. p1.x) (p2.y -. p1.y)
