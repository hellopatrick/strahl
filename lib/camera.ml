open Core

type t = {
    origin: Vec3.t;
    lower_left: Vec3.t;
    horizontal: Vec3.t;
    vertical: Vec3.t;
    u: Vec3.t;
    v: Vec3.t;
    w: Vec3.t;
    lens_radius: float;
}

let create from at up fov aspect aperture focus_dist =
    let open Float in
    let theta = fov * pi / 180.0 in
    let half_height = tan (theta / 2.0) in
    let half_width = aspect * half_height in
    let origin = from in
    let w = Vec3.(to_unit (from - at)) in
    let u = Vec3.(to_unit (cross up w)) in
    let v = Vec3.cross w u in
    {
        origin;
        lower_left = Vec3.(from - (focus_dist *. half_width * u) - (focus_dist *. half_height * v) - (focus_dist * w));
        horizontal = Vec3.(2.0 *. focus_dist *. half_width * u);
        vertical = Vec3.(2.0 *. focus_dist *. half_height * v);
        w;
        u;
        v;
        lens_radius = aperture / 2.0
    }

let random_in_disc radius =
    let open Float in
    let r = Random.float radius in
    let theta = Random.float (2.0 *. pi) in
    let x = r *. (cos theta)
    and y = r *. (sin theta)
    and z = 0.0 in
    Vec3.{x; y; z}

let get_ray camera s t =
    let rd = random_in_disc camera.lens_radius in
    let offset = Vec3.(rd.x * camera.u + rd.y * camera.v) in
    let origin = Vec3.(camera.origin + offset) in
    let direction = Vec3.(camera.lower_left + (s * camera.horizontal) + (t * camera.vertical) - camera.origin - offset) in
    Ray.{origin; direction}