module Adder (S : Common.GATES) = struct
  open S
  let ( @ ) = connect
  let wire =
      ccnot ~control1:0 ~control2:1 3
    @ cnot ~control:0 1
    @ ccnot ~control1:1 ~control2:2 3
    @ cnot ~control:1 2
    @ cnot ~control:0 1
end

let run_adder =
  let open Adder(Naieve) in
  wire

let show_adder () =
  let open Adder(Render) in
  Render.show wire

module All (S : Common.GATES) = struct
  open S
  let ( $ ) = connect
  (* to reduce stack depth, in naieve we use right assoc op and here left assoc for speed *)
  (* unfortunately we cannot always know which will be done, or both will be done *)
  let wire =
    init ~nbits:4 5
    $ pauli_x 0
    $ cnot ~control: 1 3
    $ cz ~control:2 1
    $ swap 0 1
    $ swap 1 3
    $ measure 2
end

let show_all () =
  let open All(Render) in
  Render.show wire
