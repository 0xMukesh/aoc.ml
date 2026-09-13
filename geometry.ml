open Base

type circle =
  { center : Point2d.t
  ; radius : float
  }

type rectangle =
  { lower_left : Point2d.t
  ; width : float
  ; height : float
  }

type segment =
  { endpoint_1 : Point2d.t
  ; endpoint_2 : Point2d.t
  }

type scene_element =
  | Circle of circle
  | Rectangle of rectangle
  | Segement of segment

let is_inside_scene_element (point : Point2d.t) (scene_el : scene_element) : bool =
  let open Float.O in
  match scene_el with
  | Circle { center; radius } -> Point2d.distance center point < radius
  | Rectangle { lower_left; width; height } ->
    let px, py, rx, ry =
      Point2d.x point, Point2d.y point, Point2d.x lower_left, Point2d.y lower_left
    in
    px > rx && px < rx + width && py > ry && py < ry + height
  | Segement _ -> false
;;
