open Core

type t = {x:float; y:float; z:float}

let ( * ) s {x; y; z} = {x = x *. s; y = y *. s; z = z *. s}
let ( / ) {x; y; z} s = {x = x /. s; y = y /. s; z = z /. s}

let ( + ) a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z}
let ( - ) a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z}

let multiply a b = {x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z}
let divide a b = {x = a.x /. b.x; y = a.y /. b.y; z = a.z /. b.z}

let origin = {x = 0.0; y = 0.0; z = 0.0}

let create x y z = {x; y; z}

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let norm a = dot a a

let cross a b =
    let open Float in
    let x = a.y * b.z - a.z * b.y
    and y = a.z * b.x - a.x * b.z
    and z = a.x * b.y - a.y * b.x in
    {x; y; z}

let length a = norm a |> sqrt

let to_unit a = a / (length a)

let reflect a n = a - (2.0 *. (dot a n) * n)

let to_string {x; y; z} =
    let x = Float.to_string x in
    let y = Float.to_string y in
    let z = Float.to_string z in
    String.concat [x;",";y;",";z]

let random () =
    let open Float in
    let theta = 2.0 * pi * Random.float 1.0 in
    let phi = acos (1.0 - 2.0 * Random.float 1.0) in
    let x = (sin phi) * (cos theta) in
    let y = (sin phi) * (sin theta) in
    let z = (cos phi) in
    {x; y; z;}