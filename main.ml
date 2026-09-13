open Stdio

let demo_01 () =
  let p1 = Point2d.create (0., 0.) in
  let p2 = Point2d.create (3., 4.) in
  let circle = Geometry.{ center = p1; radius = 3.0 } in
  printf "p1: %s\n" (Point2d.to_string p1);
  printf "p2: %s\n" (Point2d.to_string p2);
  printf "circle: center=%s, radius=%f\n" (Point2d.to_string circle.center) circle.radius;
  printf "distance: %f\n" (Point2d.distance p1 p2);
  printf
    "is within circle: %b\n"
    (Geometry.is_inside_scene_element p2 (Geometry.Circle circle))
;;

let rec demo_02 accum =
  match In_channel.input_line In_channel.stdin with
  | None -> accum
  | Some l ->
    (match Float.of_string_opt l with
     | None -> accum
     | Some n -> demo_02 (accum +. n))
;;

let () =
  (* %! is a flush directive for ocaml's printf *)
  printf "choose either 1 or 2\n%!";
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some "1" -> demo_01 ()
  | Some "2" -> printf "%f\n" (demo_02 0.)
  | Some c -> printf "unknown option: %s\n" c
;;
