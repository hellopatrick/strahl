open Core

module type S = sig
    type t
    val hit : t -> Ray.t -> Hit.t option
end

module type M = sig
    type t
    val scatter : t -> Ray.t -> Hit.t -> Trace.t option
end

module type Surface = sig
    module Shape : S
    module Material : M
    val this : Shape.t
    val appearance : Material.t
end

let build (type a) (type b)
(module Shape : S with type t = a) s
(module Material : M with type t = b) m =
    (module struct
      module Shape = Shape
      module Material = Material
      let this = s
      let appearance = m
    end : Surface)

let detect_hit ray (module Surface : Surface) =
    Surface.Shape.hit Surface.this ray

let trace ray (module Surface : Surface) =
    let hit = Surface.Shape.hit Surface.this ray in
    match hit with
    | None -> None
    | Some hit -> Surface.Material.scatter Surface.appearance ray hit