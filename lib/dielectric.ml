open Core

type t = { ri: float }

let attenuation = Vec3.{x = 1.0; y = 1.0; z = 1.0}

let schlick cosine ri =
    let r = (1.0 -. ri) /. (1.0 +. ri) in
    let r = r *. r in
    r +. (1.0 -. r) *. Float.int_pow (1.0 -. cosine) 5

let refract v n ri =
    let unit = Vec3.to_unit v in
    let dt = Vec3.dot unit n in
    let discriminant = 1.0 -. (ri *. ri *. (1.0 -. dt *. dt)) in
    if discriminant > 0.0 then Some Vec3.(ri * (unit - dt * n) - (sqrt discriminant) * n)
    else None

let scatter material (ray:Ray.t) (hit:Hit.t) =
    let d = Vec3.dot ray.direction hit.n > 0.0 in
    let normal = if d then Vec3.(-1.0 * hit.n) else hit.n in
    let ri = if d then material.ri else 1.0 /. material.ri in
    let cosine' = (Vec3.dot ray.direction hit.n) /. (Vec3.length ray.direction) in
    let cosine = if d then material.ri *. cosine' else -.cosine' in
    let refraction = refract ray.direction normal ri in
    let reflection = Vec3.reflect ray.direction hit.n in
    let direction =
        match refraction with
        | None -> reflection
        | Some v ->
            let prob = schlick cosine ri in
            match (Random.float 1.0) < prob with
            | true -> reflection
            | false -> v in
    Some Trace.{hit; attenuation; reflection = Ray.{origin = hit.p; direction};}