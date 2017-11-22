type kind = Light | Bounce

type t = {
    hit: Hit.t;
    attenuation: Vec3.t;
    reflection: Ray.t;
    kind: kind
}

let compare a b =
    let open Hit in
    compare a.hit b.hit
