open Base

module Expression : sig
  val simplify : Interp.sp_t -> Interp.sp_t
  val compile : Hardcaml.Signal.t Map.M(Int).t -> Interp.sp_t -> Hardcaml.Signal.t
end

module Statement : sig
  val simplify : Interp.sp_st -> Interp.sp_st
end

val compile : Interp.sp_st -> unit
