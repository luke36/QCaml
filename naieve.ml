open Utils

type t = Common.t -> Common.t

let init ~nbits:_ x wf =
  let q = Array.make (Array.length wf) Complex.zero in
  q.(x) <- Complex.one;
  q

(* [ 0 1   *)
(*   1 0 ] *)
let pauli_x i wf =
  Array.init (Array.length wf)
    (fun k -> wf.(rev_bit ~index:i k))

(* [ 0 -i   *)
(*   i  0 ] *)
let pauli_y i wf =
  Array.init (Array.length wf)
    (fun k ->
      let j = rev_bit ~index:i k in
       if ith_bit ~index:i k = 0 then
         Complex.(~- i * wf.(j))
       else
         Complex.(i * wf.(j)))

(* [ 1  0   *)
(*   0 -1 ] *)
let pauli_z i wf =
  Array.init (Array.length wf)
    (fun k ->
      if ith_bit ~index:i k = 0 then
         wf.(k)
       else
         Complex.(~- (wf.(k))))

(* 1/(sqrt 2) *  [ 1  1   *)
(*                 1 -1 ] *)
let hardamard i wf =
  Array.init (Array.length wf)
    (fun k ->
      let j = rev_bit ~index:i k in
       let sqrt_1_2 = Complex.of_float (sqrt 2. /. 2.) in
       if ith_bit ~index:i k = 0 then
         Complex.((wf.(k) + wf.(j)) * sqrt_1_2)
       else
         Complex.(( ~- (wf.(k)) + wf.(j)) * sqrt_1_2))

(* [ 1 0   *)
(*   0 i ] *)
let phase i wf =
  Array.init (Array.length wf)
    (fun k -> if ith_bit ~index:i k = 0 then wf.(k)
      else Complex.(i * wf.(k)))

(* [ 1      0       *)
(*   0  e^(i*π/4) ] *)
let pi8 i wf =
  Array.init (Array.length wf)
    (fun k -> if ith_bit ~index:i k = 0 then wf.(k)
      else
        let sqrt_1_2 = sqrt 2. /. 2. in
        Complex.(wf.(k) * (complex ~real:sqrt_1_2 ~imagi:sqrt_1_2)))

(* [ 1 0 0 0   *)
(*   0 1 0 0   *)
(*   0 0 0 1   *)
(*   0 0 1 0 ] *)
let cnot ~control:c i wf =
  assert (c <> i);
  Array.init (Array.length wf)
    (fun k -> if ith_bit ~index:c k = 0 then wf.(k)
      else wf.(rev_bit ~index:i k))

(* [ 1 0 0  0   *)
(*   0 1 0  0   *)
(*   0 0 1  0   *)
(*   0 0 0 -1 ] *)
let cz ~control:c i wf =
  Array.init (Array.length wf)
    (fun k ->
      if ith_bit ~index:c k = 0 || ith_bit ~index:i k = 0 then
         wf.(k)
       else
         Complex.(~- (wf.(k))))

(* [ 1 0 0 0   *)
(*   0 0 1 0   *)
(*   0 1 0 0   *)
(*   0 0 0 1 ] *)
let swap i j wf =
  Array.init (Array.length wf)
    (fun k -> wf.(rev_bit ~index:i @@
      rev_bit ~index:j k))

(* omitted : too long *)
let ccnot ~control1:c1 ~control2:c2 i wf =
  assert (c1 <> i && c2 <> i);
  Array.init (Array.length wf)
    (fun k -> if ith_bit ~index:c1 k = 0 || ith_bit ~index:c2 k = 0 then wf.(k)
      else wf.(rev_bit ~index:i k))

let observe i wf =
  let len = Array.length wf in
  let pr = Array.make 2 0. in
  for k = 0 to len - 1 do
    if ith_bit ~index:i k = 0 then
      pr.(0) <- pr.(0) +. Complex.norm wf.(k)
    else
      pr.(1) <- pr.(1) +. Complex.norm wf.(k)
  done;
  if Random.float 1. < pr.(0) then
    (0,
      Array.init len
       (fun k -> if ith_bit ~index:i k = 0 then
            Complex.(wf.(k) / (of_float @@ sqrt pr.(0)))
          else
           Complex.zero))
  else
    (1,
      Array.init len
       (fun k -> if ith_bit ~index:i k <> 0 then
            Complex.(wf.(k) / (of_float @@ sqrt pr.(1)))
          else
           Complex.zero))

let measure i p =
  let (_res, now) = observe i p in now

let connect f g x = g (f x)

module Operator = struct
  let ( @ ) = connect
end
