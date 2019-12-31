open Base

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

module Int64 = struct
  type t = Int64.t [@@deriving equal, compare]

  let const = Int64.of_int
  let zero = 0L
  let one = 1L
  let ( +: ) a b = Int64.(a + b)
  let ( -: ) a b = Int64.(a - b)
  let ( *: ) a b = Int64.(a * b)
  let ( /: ) a b = Int64.(a / b)
  let ( %: ) a b = Int64.(a % b)
  let ( &: ) a b = Int64.(a land b)
  let ( |: ) a b = Int64.(a lor b)
  let ( ^: ) a b = Int64.(a lxor b)
  let ( ~: ) a = Int64.(lnot a)
  let sll a b = Int64.shift_left a (Int64.to_int_exn b)
  let srl a b = Int64.shift_right_logical a (Int64.to_int_exn b)
  let sra a b = Int64.shift_right a (Int64.to_int_exn b)
  let ( ==: ) a b = if Int64.equal a b then 1L else 0L
  let ( <>: ) a b = if not (Int64.equal a b) then 1L else 0L
  let ( <+ ) a b = if Int64.compare a b < 0 then 1L else 0L
  let ( <=+ ) a b = if Int64.compare a b <= 0 then 1L else 0L
  let ( >+ ) a b = if Int64.compare a b > 0 then 1L else 0L
  let ( >=+ ) a b = if Int64.compare a b >= 0 then 1L else 0L
  let mask = 0x8000_0000_0000_0000L
  let ( <: ) a b = a ^: mask <+ b ^: mask
  let ( <=: ) a b = a ^: mask <=+ b ^: mask
  let ( >: ) a b = a ^: mask >+ b ^: mask
  let ( >=: ) a b = a ^: mask >=+ b ^: mask
end
