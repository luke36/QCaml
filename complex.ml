type t = float * float

let square x = x *. x

let zero = (0., 0.)
let one = (1., 0.)
let i = (0., 1.)
let ( + ) (a, b) (c, d) = (a +. c, b +. d)
let ( - ) (a, b) (c, d) = (a -. c, b -. d)
let ( * ) (a, b) (c, d) = (a *. c -. b *. d,a *. d +. b *. c)
let ( / ) (a, b) (c, d) =
  let t = square c +. square d in
    ((a *. c +. b *. d) /. t, (b *. c -. a *. d) /. t)
let ( ~- ) (a, b) = (a, -. b)
let norm (a, b) = square a +. square b
let abs z = sqrt @@ norm z
let real (a, _b) = a
let imagi (_a, b) = b
let complex ~real:a ~imagi:b = (a, b)
let of_exp ~radius:r ~angle:th = (r *. cos th, r *. cos th)
let of_float r = (r, 0.)
