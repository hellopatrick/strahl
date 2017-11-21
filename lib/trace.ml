type t = {
    hit: Hit.t;
    attenuation: Vec3.t;
    reflection: Ray.t;
}

let compare h i =
    let open Hit in
    compare h.hit i.hit
