let ith_bit ~index:i x = x land (1 lsl i)
let rev_bit ~index:i x = x lxor (1 lsl i)
                           
let square x = x * x

let rec pow a b =
  if b = 0 then 1
  else if b mod 2 = 0 then
    square (pow a (b / 2))
  else
    a *  pow a (b - 1)

