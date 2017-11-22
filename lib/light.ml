type t = { color : Vec3.t }

let scatter material (ray:Ray.t) (hit:Hit.t) =
    let scattered = Ray.{origin = hit.p; direction = Vec3.origin} in
    Some Trace.{hit = hit; attenuation = material.color; reflection = scattered; kind = Trace.Light }