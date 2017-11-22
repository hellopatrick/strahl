open Core

module Vec3 = Vec3
module Ray = Ray
module Surface = Surface
module Sphere = Sphere
module Plane = Plane
module Matte = Matte
module Metal = Metal
module Dielectric = Dielectric
module Camera = Camera
module Trace = Trace
module Light = Light

let black = Vec3.{x = 0.01; y = 0.01; z = 0.01}
let white = Vec3.{x = 1.0; y = 1.0; z = 1.0}

let background_color ray =
    black

let rec trace_ray_aux world ray = function
    | 0 -> Vec3.origin
    | n ->
        let traces = List.filter_map world ~f:(Surface.trace ray) in
        let best_trace = List.min_elt traces ~cmp:Trace.compare in
        match best_trace with
        | None -> background_color ray
        | Some trace ->
            match trace with
            | Trace.{ kind = Light; _} -> trace.attenuation
            | Trace.{ kind = Bounce; _ } -> Vec3.multiply trace.attenuation (trace_ray_aux world trace.reflection (n-1))

let rec trace world ray =
    trace_ray_aux world ray 16
