module type S = sig
  type t [@@deriving equal, compare]

  val const : int -> t
  val zero : t
  val one : t
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t
  val ( %: ) : t -> t -> t
  val ( &: ) : t -> t -> t
  val ( |: ) : t -> t -> t
  val ( ^: ) : t -> t -> t
  val ( ~: ) : t -> t
  val sll : t -> t -> t
  val srl : t -> t -> t
  val sra : t -> t -> t
  val ( ==: ) : t -> t -> t
  val ( <>: ) : t -> t -> t
  val ( <+ ) : t -> t -> t
  val ( <=+ ) : t -> t -> t
  val ( >+ ) : t -> t -> t
  val ( >=+ ) : t -> t -> t
  val ( <: ) : t -> t -> t
  val ( <=: ) : t -> t -> t
  val ( >: ) : t -> t -> t
  val ( >=: ) : t -> t -> t
end

module Int64 : S with type t = int64
