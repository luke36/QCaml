type t =
  | Init of int * int
  | PauliX of int
  | PauliY of int
  | PauliZ of int
  | Hardamard of int
  | Phase of int
  | Pi8 of int
  | CNot of int * int
  | CZ of int * int
  | Swap of int * int
  | CCNot of int * int * int
  | Measure of int
  | Connect of t * t

let init ~nbits:n v = Init (n, v)
let pauli_x i = PauliX i
let pauli_y i = PauliY i
let pauli_z i = PauliZ i
let hardamard i = Hardamard i
let phase i = Phase i
let pi8 i = Pi8 i
let cnot ~control:c i = CNot (c, i)
let cz ~control:c i = CZ (c, i)
let swap i j = Swap (i, j)
let ccnot ~control1:c1 ~control2:c2 i = CCNot (c1, c2, i)
let measure i = Measure i
let connect g1 g2 = Connect (g1, g2)

let inst w : (module Common.WIRE) =
  let module W (G : Common.GATES) = struct
    let wire =   
      let open G in
      let rec recur t =
        match t with
        | Init (n, v) -> init ~nbits:n v
        | PauliX i -> pauli_x i
        | PauliY i -> pauli_y i
        | PauliZ i -> pauli_z i
        | Hardamard i -> hardamard i
        | Phase i -> phase i
        | Pi8 i -> pi8 i
        | CNot (c, i) -> cnot ~control:c i
        | CZ (c, i) -> cz ~control:c i
        | Swap (i, j) -> swap i j
        | CCNot (c1, c2, i) -> ccnot ~control1:c1 ~control2:c2 i
        | Measure i -> measure i
        | Connect (a, d) -> connect (recur a) (recur d)  
      in recur w
  end in
  (module W)
