open Base
open Stdio

let rec sum l =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let rec remove_sequential_duplicates l =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | first :: second :: tl ->
    if first = second
    then remove_sequential_duplicates (second :: tl)
    else first :: remove_sequential_duplicates (second :: tl)
;;

let () = printf "%d\n" (sum [ 1; 2; 3 ])
