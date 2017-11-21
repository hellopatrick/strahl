open Core

type t = { origin: Vec3.t; direction:Vec3.t }

let create origin direction = { origin; direction }

let point_at ray time =
    Vec3.(ray.origin + (time * ray.direction))

let unit_direction ray =
    Vec3.to_unit ray.direction

