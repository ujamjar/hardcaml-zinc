open Hardcaml
open Signal

type 'a prefix = ('a -> 'a -> 'a) -> 'a list -> 'a list

module Make (B : Comb.S) : sig
  val arbiter : prefix:B.t prefix -> unmasked_req:B.t -> masked_req:B.t -> B.t * B.t
end

val arbiter : prefix:t prefix -> enable:t -> req:t -> t

module Test : sig
  val test : prefix:t prefix -> bits:int -> unit
end
