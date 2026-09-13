open Base
open Stdio

module Point2d = struct
  type t =
    { x : float
    ; y : float
    }

  let create (x, y) : t = { x; y }
  let magnitude (point : t) : float = Float.sqrt ((point.x **. 2.) +. (point.y **. 2.))
end

let rec sum (l : int list) : int =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let rec remove_sequential_duplicates (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | first :: second :: tl ->
    if first = second
    then remove_sequential_duplicates (second :: tl)
    else first :: remove_sequential_duplicates (second :: tl)
(* disables warning 32 which related to unused variable declaration *)
[@@warning "-32"]
;;

let downcase_ext (filename : string) : string =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext
;;

let () =
  printf "%d\n" (sum [ 1; 2; 3 ]);
  (* `fun` is used for creating anonymous functions *)
  List.iter [ "Hello_world"; "hello_world.txt"; "hello_world.MP4" ] ~f:(fun filename ->
    printf "%s\n" (downcase_ext filename));
  printf "%f\n" (Point2d.magnitude (Point2d.create (2.0, 3.0)))
;;
