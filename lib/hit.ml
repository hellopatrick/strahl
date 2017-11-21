open Core

type t = {
    t : float;
    p : Vec3.t;
    n : Vec3.t;
}

let compare h i =
    let open Float in
    compare h.t i.t