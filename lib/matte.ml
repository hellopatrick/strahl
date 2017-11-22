type t = { color : Vec3.t }

let scatter material (ray:Ray.t) (hit:Hit.t) =
    let target = Vec3.(hit.p + hit.n + random ()) in
    let scattered = Ray.{origin = hit.p; direction = Vec3.(target - hit.p)} in
    Some Trace.{hit = hit; attenuation = material.color; reflection = scattered; kind = Trace.Bounce}