type t

val fresh : int -> t
val pauli_x : t -> t
val pauli_y : t -> t
val pauli_z : t -> t
val hardamard : t -> t
val phase : t -> t
val pi8 : t -> t
val cnot : control:t -> t -> t
val cz : control:t -> t -> t
val swap : t -> t -> t * t
val ccnot : control1:t -> control2:t -> t -> t
val observe : t -> int
val measure : t -> t
