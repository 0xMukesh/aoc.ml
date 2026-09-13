open Stdio

let () =
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
