open Base
open Hardcaml

module Expression : sig
  val simplify : Interp.sp_t -> Interp.sp_t
  val compile : (int -> Signal.t) -> Interp.sp_t -> Hardcaml.Signal.t
end

module Statement : sig
  val simplify : Interp.sp_st -> Interp.sp_st

  module Usage : sig
    type t =
      { read_registers : Machine.Register.t list
      ; write_registers : Machine.Register.t list
      ; read_memories : Machine.Cache.t list
      ; write_memories : Machine.Cache.t list
      }

    val create : Interp.sp_cmd list -> t
  end
end

module Sequential : sig
  module Zinc_register : sig
    include Hardcaml.Interface.S

    val create : Reg_spec.t -> Always.Variable.t t
    val var : Always.Variable.t t -> Machine.Register.t -> Always.Variable.t
    val get : Always.Variable.t t -> Machine.Register.t -> Signal.t
  end

  module Command_register : sig
    type t

    val create : Reg_spec.t -> t
    val var : t -> int -> Always.Variable.t
  end

  module Memory_control : sig
    module I : sig
      type 'a t =
        { read_available : 'a
        ; read_data : 'a
        ; write_complete : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { read : 'a
        ; read_address : 'a
        ; write : 'a
        ; write_data : 'a
        ; write_address : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_in : 'a Memory_control.I.t
      ; program_in : 'a Memory_control.I.t
      ; stack_in : 'a Memory_control.I.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { memory_out : 'a Memory_control.O.t
      ; program_out : 'a Memory_control.O.t
      ; stack_out : 'a Memory_control.O.t
      ; zinc_registers : 'a Zinc_register.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val compile : Interp.sp_st -> Signal.t Interface.Create_fn(I)(O).t
end
