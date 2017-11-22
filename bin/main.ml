open Core
open Strahl

module Out = Out_channel

let ball = Sphere.{center = Vec3.{x = 0.0; y = 0.5; z = -0.0}; radius = 0.5}
let another_ball = Sphere.{center = Vec3.{x = 0.2; y = 0.75; z = -2.0}; radius = 0.75}
let yet_another_ball = Sphere.{center = Vec3.{x = 0.75; y = 0.25; z = -0.0}; radius = 0.25}
let left_ball = Sphere.{center = Vec3.{x = -1.5; y = 0.5; z = -1.0}; radius = 0.5}
let inside_left_ball = Sphere.{center = Vec3.{x = -1.5; y = 0.5; z = -1.0}; radius = -0.48}
let back_left_ball = Sphere.{center = Vec3.{x = -2.0; y = 1.0; z = -2.5}; radius = 1.0}
let right_ball = Sphere.{center = Vec3.{x = 1.25; y = 0.5; z = -1.0}; radius = 0.5}
let ground = Plane.{normal = Vec3.{x = 0.0; y = 1.0; z = 0.0}; origin = Vec3.{x = 0.0; y = 0.0; z = 0.0}}
let more_ball = Sphere.{center = Vec3.{x = 1.35; y = 0.35; z = -0.0}; radius = 0.35}
let lamp = Sphere.{center = Vec3.{x = -2.0; y = 0.5; z = -0.0}; radius = 0.5}

let dark = Matte.{color = Vec3.{x = 0.5; y = 0.5; z = 0.5}}
let light = Matte.{color = Vec3.{x = 0.9; y = 0.9; z = 0.9}}
let green = Matte.{color = Vec3.{x = 0.5; y = 0.9; z = 0.3}}
let red = Matte.{color = Vec3.{x = 1.000; y = 0.078; z = 0.576}}
let diamond = Dielectric.{ri = 2.4}
let mirror = Metal.{color = Vec3.{x = 0.902; y = 0.902; z = 0.980}; fuzz = 0.05}
let source = Light.{color = Vec3.{x = 1.0; y = 0.0; z = 0.0}}

let world = [
    Surface.build (module Sphere) ball (module Matte) green;
    Surface.build (module Sphere) another_ball (module Matte) light;
    Surface.build (module Sphere) yet_another_ball (module Matte) red;
    Surface.build (module Plane) ground (module Matte) light;
    Surface.build (module Sphere) left_ball (module Dielectric) diamond;
    Surface.build (module Sphere) inside_left_ball (module Dielectric) diamond;
    Surface.build (module Sphere) right_ball (module Metal) mirror;
    Surface.build (module Sphere) back_left_ball (module Metal) mirror;
    Surface.build (module Sphere) more_ball (module Dielectric) diamond;
    Surface.build (module Sphere) lamp (module Light) source;
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
    let open Float in
    let x = (Random.float 1.0) + of_int x
    and y = (Random.float 1.0) + of_int y
    and w = of_int w
    and h = of_int h in
    Camera.get_ray camera (x / w) (y / h)

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
    let from = Vec3.{x = -2.0; y = 2.0; z = 3.0}
    and at = Vec3.{x = 0.0; y = 0.0; z = -1.0}
    and up = Vec3.{x = 0.0; y = 1.0; z = 0.0}
    and fov = 45.0
    and aspect_ratio = 1.7777
    and aperture = 0.01 in
    let focus_dist = Vec3.(length (from - at)) in
    Camera.create from at up fov aspect_ratio aperture focus_dist

let () =
    let w, h = 640, 360 in
    print_endline "tracing...";
    let image = Array.init h ~f:(fun y ->
        let y = h - y in
        Array.init w ~f:(fun x ->
            trace camera 600 x y w h
        )
    ) in
    print_endline "saving...";
    output "./out/image.ppm" image (w, h)