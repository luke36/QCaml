open Utils

type range = { image : string array; from : int; til : int }

type t = range list list

let overlap r1 r2 = r1.from <= r2.til && r2.from <= r1.til
let fall_in x r = r.from <= x && x <= r.til

let init ~nbits:n v =
  let img = Array.init (3 * n)
    (fun i ->
        if i mod 3 = 1 then
          if ith_bit ~index:(i/3) v = 0 then
            "─ 0 ─"
          else
            "─ 1 ─"
        else
          "     " ) in
  [ [ { image = img; from = 0; til = n - 1 } ] ]

let pauli_x i =
  [ [ { image = [|
        "┌───┐";
        "┤ X ├";
        "└───┘" |]; from = i; til = i } ] ]

let pauli_y i =
  [ [ { image = [|
        "┌───┐";
        "┤ Y ├";
        "└───┘" |]; from = i; til = i } ] ]

let pauli_z i =
  [ [ { image = [|
        "┌───┐";
        "┤ Z ├";
        "└───┘" |]; from = i; til = i } ] ]

let hardamard i =
  [ [ { image = [|
        "┌───┐";
        "┤ H ├";
        "└───┘" |]; from = i; til = i } ] ]

let phase i =
  [ [ { image = [|
        "┌───┐";
        "┤ S ├";
        "└───┘" |]; from = i; til = i } ] ]

let pi8 i =
  [ [ { image = [|
        "┌───┐";
        "┤ T ├";
        "└───┘" |]; from = i; til = i } ] ]

let two_node_aux b e b_img e_img =
  let len = 3 * (e - b + 1) in
  let img = Array.init len
      (fun k ->
         if k mod 3 = 1 then "────"
         else "  │  ") in
  for i = 0 to 2 do
    img.(i) <- b_img.(i)
  done;
  for i = 0 to 2 do
    img.(len - 3 + i) <- e_img.(i)
  done;
  [ [ { image = img; from = b; til = e } ] ]

let cnot ~control:c i =
  assert (c <> i);
  if c < i then
    two_node_aux c i
      [| "     ";
         "────";
         "  │  " |]
      [| "  │  ";
         "──⊕──";
         "     " |]
  else
    two_node_aux i c
      [| "     ";
         "──⊕──";
         "  │  " |]
      [| "  │  ";
         "────";
         "     " |]

let cz ~control:c i =
  if c = i then pauli_z i
  else if c < i then
    two_node_aux c i
      [| "     ";
         "────";
         "  │  " |]
      [| "┌─┴─┐";
         "┤ Z ├";
         "└───┘" |]
  else
    two_node_aux i c
      [| "┌───┐";
         "┤ Z ├";
         "└─┬─┘" |]
      [| "  │  ";
         "────";
         "     " |]

let swap i j =
  if i = j then []
  else
    let i' = Int.min i j in
    let j' = Int.max i j in
    if j' = i' + 1 then
      two_node_aux i' j'
        [| "     ";
           "  ─";
           " ╲╱  " |]
        [| " ╱╲  " ;
           "  ─";
           "     " |]
      else    
      two_node_aux i' j'
        [| "     ";
           "────";
           "  │  " |]
        [| "  │  ";
           "────";
           "     " |]

let ccnot ~control1:_c1 ~control2:_c2 _i = [] (* TODO *)

let measure i =
  [ [ { image = [|
        "     ";
        "───";
        "     " |]; from = i; til = i } ] ]

let fit r g =
  let no_overlap_all r1 rs =
    not @@ List.exists (fun r -> overlap r r1) rs in
  let rec fit_succ r g =
    match g with
    | [] -> None
    | rs :: g' ->
      if no_overlap_all r rs then
          match fit_succ r g' with
            | Some g'' -> Some (rs :: g'')
            | None -> Some ((r :: rs) :: g')
        else None
  in
  match (fit_succ r g) with
  | Some g -> g
  | None -> [r] :: g
    
let rec fits c g =
  match c with
  | [] -> g
  | a :: d -> fits d (fit a g)
    
let connect g1 g2 =
  let g1' = List.rev g1 in
  let rec loop g1 g2 =
    match g2 with
    | [] -> g1
    | a :: d -> loop (fits a g1) d
  in
  List.rev @@ loop g1' g2
    
let show g =
  let max_bit =
    List.fold_left
      (fun acc l -> Int.max acc @@
        List.fold_left (fun acc r -> Int.max acc (Int.max r.from r.til)) 0 l) 0 g in
  for line = 0 to 3 * max_bit + 2 do
    if line mod 3 = 1 then print_string "───"
    else print_string "   ";
    let ibit = line / 3 in
    List.iter
      (fun rs ->
        (match List.find_opt (fun r -> fall_in ibit r) rs with
        | Some r -> print_string r.image.(line - r.from * 3)
        | None ->
            if line mod 3 = 1 then print_string "─────"
            else print_string "     ");
        if line mod 3 = 1 then print_string "─────"
        else print_string "     "
      ) g;
    print_newline ();  
  done

module Operator = struct
  let ( $ ) = connect
end
