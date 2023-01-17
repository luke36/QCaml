type group = { nbits : int; wf : Complex.t array; bits : t array }

and t = { mutable group : group; mutable index : int }

let dummy_grp = { nbits = 0; wf = [||]; bits = [||] }

let fresh v =
  let b = { group = dummy_grp; index = 0 } in
  let wf = if v = 0 then Complex.([| one; zero |])
    else Complex.([| zero; one |]) in
  b.group <- { nbits = 1; wf; bits = [| b |] };
  b

let merge g1 g2 =
  let n1 = g1.nbits
  and n2 = g2.nbits in
  let wf = Common.make ~bits:(n1 + n2) in
  for i = 0 to Array.length g1.wf - 1 do
    for j = 0 to Array.length g2.wf - 1 do
      wf.(i + j lsl n1) <- Complex.( * ) g1.wf.(i) g2.wf.(j)
    done
  done;
  { nbits = n1 + n2; wf; bits = Array.append g1.bits g2.bits }

let rectangle b1 b2 =
  if b1.group != b2.group then
    let g = merge b1.group b2.group in
    let n1 = b1.group.nbits in
    Array.iter (fun b -> b.group <- g) b1.group.bits;
    Array.iter (fun b -> b.group <- g; b.index <- b.index + n1) b2.group.bits

(* a <- b *)
let copy_content a b =
  for i = 0 to Array.length a - 1 do
    a.(i) <- b.(i)
  done

let onebit_aux f =
  fun b ->
  let wf' = f b.index b.group.wf in
  copy_content b.group.wf wf';
  b
let pauli_x = onebit_aux Naieve.pauli_x
let pauli_y = onebit_aux Naieve.pauli_y
let pauli_z = onebit_aux Naieve.pauli_z
let hardamard = onebit_aux Naieve.hardamard
let phase = onebit_aux Naieve.phase
let pi8 = onebit_aux Naieve.pi8

let twobit_aux f =
  fun b1 b2 ->
  rectangle b1 b2;
  let wf' = f b1.index b2.index b1.group.wf in
  copy_content b1.group.wf wf';
  (b1, b2)
let cnot ~control:b1 b2  = snd @@ twobit_aux (fun b1 b2 wf -> Naieve.cnot ~control:b1 b2 wf) b1 b2
let cz ~control:b1 b2 = snd @@ twobit_aux (fun b1 b2 wf -> Naieve.cz ~control:b1 b2 wf) b1 b2
let swap = twobit_aux Naieve.swap

let ccnot ~control1:b1 ~control2:b2 b3 =
  rectangle b1 b2;
  rectangle b1 b3;  
  let wf' = Naieve.ccnot ~control1:b1.index ~control2:b2.index b3.index b1.group.wf in
  copy_content b1.group.wf wf';
  b3

let compact i wf =
  Array.init (Array.length wf / 2)
    (fun k ->
      let upper = (k lsr i) lsl i in
       let j = k + upper in (* 101 2 -> 1001 *)
       let l = k + upper + 1 lsl i in  (* 101 2 -> 1101 *)
       Complex.(wf.(j) + wf.(l)))

let observe b =
  let (v, wf') = Naieve.observe b.index b.group.wf in
  let wf'' = compact b.index wf' in
  let g' = { nbits = b.group.nbits - 1;
             wf = wf'';
             bits = Array.init (Array.length b.group.bits - 1)
          (fun k -> if k < b.index then b.group.bits.(k) else b.group.bits.(k + 1)) }
  in
  Array.iter (fun c -> c.group <- g'; if c.index > b.index then c.index <- c.index - 1) b.group.bits;
  v

let measure b = fresh @@ observe b
