module type S = sig

  type t
  val const : int -> t
  val zero : t
  val one : t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  
  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t
  val ( %: ) : t -> t -> t

  val (&:) : t -> t -> t
  val (|:) : t -> t -> t
  val (^:) : t -> t -> t
  val (~:) : t -> t

  val sll : t -> int -> t
  val srl : t -> int -> t
  val sra : t -> int -> t

  val (==:) : t -> t -> t
  val (<>:) : t -> t -> t
  val (<:) : t -> t -> t
  val (<=:) : t -> t -> t
  val (>:) : t -> t -> t
  val (>=:) : t -> t -> t

end

module Int64 : S
  with type t = int64

