type t = float * float

val zero : t
val one : t
val i : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( ~- ) : t -> t
val norm : t -> float
val abs : t -> float
val real : t -> float
val imagi : t -> float
val complex : real:float -> imagi:float -> t
val of_exp : radius:float -> angle:float -> t
val of_float : float -> t
