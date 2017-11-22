type t = { color : Vec3.t; fuzz : float }

let scatter material (ray:Ray.t) (hit:Hit.t) =
    let dir = Ray.unit_direction ray in
    let reflect_dir = Vec3.reflect dir hit.n in
    let shift = Vec3.(material.fuzz * random ()) in
    let scattered = Ray.{origin = hit.p; direction = Vec3.(reflect_dir + shift)} in
    if Vec3.dot reflect_dir hit.n <= 0.0 then None
    else Some Trace.{hit = hit; attenuation = material.color; reflection = scattered; kind = Trace.Bounce}