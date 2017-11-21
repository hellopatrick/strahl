open Core

module Ray = Ray

type t = { center: Vec3.t; radius: float }

let create (center, radius) =
    {center; radius}

let produce_hit sphere ray t =
    let p = Ray.point_at ray t in
    let n = Vec3.((p -  sphere.center) / sphere.radius) in
    Hit.{t; p; n}


let hit sphere (ray:Ray.t) =
    let oc = Vec3.(ray.origin - sphere.center) in
    let a = Vec3.norm ray.direction in
    let b = 2.0 *. (Vec3.dot ray.direction oc) in
    let c = Vec3.norm oc -. (sphere.radius *. sphere.radius) in
    let discriminant = (b *. b -. 4.0 *. a *. c) in
        if discriminant <= 0.0 then None
        else
            let d = sqrt discriminant in
            let t = (-.b -. d) /. (2.0 *. a) in
            if t > 0.001 && t < Float.max_finite_value then Some (produce_hit sphere ray t)
            else
                let t = (-.b +. d) /. (2.0 *. a) in
                if t > 0.001 && t < Float.max_finite_value then Some (produce_hit sphere ray t)
                else None