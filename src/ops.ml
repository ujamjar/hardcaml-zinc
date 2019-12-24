module type S = sig
  type t

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

module Int64 = struct
  open Int64

  type t = int64

  let const = of_int
  let zero = 0L
  let one = 1L
  let ( +: ) a b = add a b
  let ( -: ) a b = sub a b
  let ( *: ) a b = mul a b
  let ( /: ) a b = div a b
  let ( %: ) a b = rem a b
  let ( &: ) a b = logand a b
  let ( |: ) a b = logor a b
  let ( ^: ) a b = logxor a b
  let ( ~: ) a = lognot a
  let sll a b = shift_left a (to_int b)
  let srl a b = shift_right_logical a (to_int b)
  let sra a b = shift_right a (to_int b)
  let ( ==: ) a b = if a = b then 1L else 0L
  let ( <>: ) a b = if a <> b then 1L else 0L
  let ( <+ ) a b = if Int64.compare a b = -1 then 1L else 0L
  let ( <=+ ) a b = if Int64.compare a b <= 0 then 1L else 0L
  let ( >+ ) a b = if Int64.compare a b = 1 then 1L else 0L
  let ( >=+ ) a b = if Int64.compare a b >= 0 then 1L else 0L
  let mask = 0x8000_0000_0000_0000L
  let ( <: ) a b = a ^: mask <+ b ^: mask
  let ( <=: ) a b = a ^: mask <=+ b ^: mask
  let ( >: ) a b = a ^: mask >+ b ^: mask
  let ( >=: ) a b = a ^: mask >=+ b ^: mask
end
