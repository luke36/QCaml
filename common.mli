type t = Complex.t array

val make : bits:int -> Complex.t array

module type GATES = sig
  type t

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
end

module type WIRE =
  functor (G : GATES) -> sig
    val wire : G.t
  end
