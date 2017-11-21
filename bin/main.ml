open Core
open Strahl

module Out = Out_channel

let ball = Sphere.{center = Vec3.{x = 0.0; y = 0.0; z = -1.0}; radius = 0.5}
let left_ball = Sphere.{center = Vec3.{x = -1.0; y = 0.0; z = -1.0}; radius = 0.5}
let back_left_ball = Sphere.{center = Vec3.{x = -1.0; y = 0.0; z = -2.0}; radius = 0.5}
let inside_left_ball = Sphere.{center = Vec3.{x = -1.0; y = 0.0; z = -1.0}; radius = -0.45}
let right_ball = Sphere.{center = Vec3.{x = 1.0; y = 0.0; z = -1.0}; radius = 0.5}
let ground = Sphere.{center = Vec3.{x = 0.0; y = -200.5; z = -1.0}; radius = 200.0}

let black = Matte.{color = Vec3.{x = 0.2; y = 0.2; z = 0.2}}
let green = Matte.{color = Vec3.{x = 0.3; y = 0.9; z = 0.4}}
let diamond = Dielectric.{ri = 2.4}
let mirror = Metal.{color = Vec3.{x = 1.0; y = 0.8; z = 1.0}; fuzz = 0.01}

let world = [
    Surface.build (module Sphere) ball (module Matte) green;
    Surface.build (module Sphere) ground (module Matte) black;
    Surface.build (module Sphere) left_ball (module Dielectric) diamond;
    Surface.build (module Sphere) inside_left_ball (module Dielectric) diamond;
    Surface.build (module Sphere) right_ball (module Metal) mirror;
    Surface.build (module Sphere) back_left_ball (module Metal) mirror;
]

let print_color file Vec3.{x; y ; z} =
    let color = Vec3.(255.0 * {x = sqrt x; y = sqrt y; z = sqrt z}) in
    fprintf file "%0.0f %0.0f %0.0f\n" color.x color.y color.z

let output ~path ~image (width, height) =
    let open Vec3 in
    let file = Out.create path in
    fprintf file "P3\n";
    fprintf file "%d %d\n" width height;
    fprintf file "255\n";
    Array.iter image ~f:(fun row ->
        Array.iter row ~f:(print_color file)
    )

let random_ray camera x y w h =
    let x, y, w, h = Float.(of_int x, of_int y, of_int w, of_int h) in
    let x' = x +. Random.float 1.0 in
    let y' = y +. Random.float 1.0 in
    Camera.get_ray camera (x' /. w) (y' /. h)

let trace camera n x y w h =
    let rec aux n acc =
        match n with
        | 0 -> acc
        | n ->
            let r = random_ray camera x y w h in
            let c = Strahl.trace world r in
            aux (n-1) Vec3.(acc + c)
    in
    let sum = aux n Vec3.origin in
    Vec3.(sum / (Float.of_int n))

let camera =
    let from = Vec3.{x = -2.0; y = 1.5; z = 1.0}
    and at = Vec3.{x = 0.0; y = 0.0; z = -1.0}
    and up = Vec3.{x = 0.0; y = 1.0; z = 0.0} in
    let fov = 35.0
    and aspect_ratio = 2.0
    and aperture = 0.05 in
    let focus_dist = Vec3.(length (from - at)) in
    Camera.create from at up fov aspect_ratio aperture focus_dist

let () =
    let w, h = 1000, 500 in
    print_endline "tracing...";
    let image = Array.init h ~f:(fun y ->
        let y = h - y in
        Array.init w ~f:(fun x ->
            trace camera 50 x y w h
        )
    ) in
    print_endline "saving...";
    output "./out/image.ppm" image (w, h)