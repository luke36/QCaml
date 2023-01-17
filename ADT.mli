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

val init : nbits:int -> int -> t
val pauli_x : int -> t
val pauli_y : int -> t
val pauli_z : int -> t
val hardamard : int -> t
val phase : int -> t
val pi8 : int -> t
val cnot : control:int -> int -> t
val cz : control:int -> int -> t
val swap : int -> int -> t
val ccnot : control1:int -> control2:int -> int -> t
val measure : int -> t
val connect : t -> t -> t
val inst : t -> (module Common.WIRE)
