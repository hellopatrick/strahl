open Core

module Vec3 = Vec3
module Ray = Ray
module Surface = Surface
module Sphere = Sphere
module Matte = Matte
module Metal = Metal
module Dieletric = Dieletric
module Camera = Camera
module Trace = Trace

let blend' = Vec3.{x = 0.5; y = 0.7; z = 1.0}
let blend = Vec3.{x = 1.0; y = 1.0; z = 1.0}

let background_color ray =
    let dir = Ray.unit_direction ray in
    let t = 0.5 *. (dir.y +. 1.0) in
    Vec3.((1.0 -. t) * blend + t * blend')

let foreground_color Hit.{p; n; t} =
    let ones = Vec3.{x = 1.0; y = 1.0; z = 1.0} in
    Vec3.(0.5 * (ones + n))

let rec trace_ray_aux world ray = function
    | 0 -> Vec3.origin
    | n ->
        let traces = List.filter_map world ~f:(Surface.trace ray) in
        let best_trace = List.min_elt traces ~cmp:Trace.compare in
        match best_trace with
        | None -> background_color ray
        | Some trace ->
            Vec3.multiply trace.attenuation (trace_ray_aux world trace.reflection (n-1))

let rec trace world ray =
    trace_ray_aux world ray 50
