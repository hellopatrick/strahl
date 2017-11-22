open Core

type t = { origin: Vec3.t; normal: Vec3.t; }

let produce_hit plane ray t =
    let p = Ray.point_at ray t in
    let n = Vec3.(plane.normal) in
    Hit.{t; p; n}

let hit plane (ray:Ray.t) =
    let denom = Vec3.dot plane.normal ray.direction in
    if Float.abs denom < 0.0001 then None
    else
        let v = Vec3.(plane.origin - ray.origin) in
        let t = (Vec3.dot v plane.normal) /. denom in
            if t > 0.0001 then Some (produce_hit plane ray t)
            else None