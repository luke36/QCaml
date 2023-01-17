let rec llinearize1 tr acc =
  match tr with
  | ADT.Connect (l, r) ->
    llinearize1 l (Some (llinearize1 r acc))
  | _ ->
    match acc with
    | None -> tr
    | Some acc -> ADT.Connect (tr, acc)

let llinearize tr = llinearize1 tr None

let rec rlinearize1 acc tr =
  match tr with
  | ADT.Connect (l, r) ->
    rlinearize1 (Some (rlinearize1 acc l)) r
  | _ ->
    match acc with
    | None -> tr
    | Some acc -> ADT.Connect (acc, tr)

let rlinearize tr = rlinearize1 None tr
                    
module LLine' (W : Common.WIRE) (G : Common.GATES)
  : sig val wire : G.t end = struct
  module I = W(ADT)
  let tree = I.wire
  let l = llinearize tree
  let wire =
    let (module W) = ADT.inst l in
    let open W(G) in
    wire
end

module RLine' (W : Common.WIRE) (G : Common.GATES)
  : sig val wire : G.t end = struct
  module I = W(ADT)
  let tree = I.wire
  let l = rlinearize tree
  let wire =
    let (module W) = ADT.inst l in
    let open W(G) in
    wire
end

module LLine (W : Common.WIRE) (G : Common.GATES)
  : sig val wire : G.t end = struct
  module G' = struct
    type t = G.t option -> G.t
  
    let helper g = function
      | None -> g
      | Some acc -> G.connect g acc
  
    let init ~nbits:n v = helper @@ G.init ~nbits:n v
    let pauli_x i = helper @@ G.pauli_x i
    let pauli_y i = helper @@ G.pauli_y i
    let pauli_z i = helper @@ G.pauli_z i
    let hardamard i = helper @@ G.hardamard i
    let phase i = helper @@ G.phase i
    let pi8 i = helper @@ G.pi8 i
    let cnot ~control:c i = helper @@ G.cnot ~control:c i
    let cz ~control:c i = helper @@ G.cz ~control:c i
    let swap i j = helper @@ G.swap i j
    let ccnot ~control1:c1 ~control2:c2 i =
      helper @@ G.ccnot ~control1:c1 ~control2:c2 i
    let measure i = helper @@ G.measure i
    let connect g1 g2 acc = g1 (Some (g2 acc))
  end
  let wire =
    let open W(G') in
    wire None
end

module RLine (W : Common.WIRE) (G : Common.GATES)
  : sig val wire : G.t end = struct
  module G' = struct
    type t = G.t option -> G.t

    let helper g = function
      | None -> g
      | Some acc -> G.connect acc g
  
    let init ~nbits:n v = helper @@ G.init ~nbits:n v
    let pauli_x i = helper @@ G.pauli_x i
    let pauli_y i = helper @@ G.pauli_y i
    let pauli_z i = helper @@ G.pauli_z i
    let hardamard i = helper @@ G.hardamard i
    let phase i = helper @@ G.phase i
    let pi8 i = helper @@ G.pi8 i
    let cnot ~control:c i = helper @@ G.cnot ~control:c i
    let cz ~control:c i = helper @@ G.cz ~control:c i
    let swap i j = helper @@ G.swap i j
    let ccnot ~control1:c1 ~control2:c2 i =
      helper @@ G.ccnot ~control1:c1 ~control2:c2 i
    let measure i = helper @@ G.measure i
    let connect g1 g2 acc = g2 (Some (g1 acc))
  end
  let wire =
    let open W(G') in
    wire None
end
