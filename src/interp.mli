open Base

module type State = sig
  type st

  include Ops.S

  (* machine registers *)

  val get_reg : st -> Machine.Register.t -> t * st
  val set_reg : st -> Machine.Register.t -> t -> st

  (* memory access *)

  val get_mem : st -> Machine.Cache.t -> t -> t * st
  val set_mem : st -> Machine.Cache.t -> t -> t -> st

  (* control *)

  val cond : st -> t -> (st -> unit * st) -> (st -> unit * st) -> st
  val iter_up : st -> t -> t -> (t -> st -> unit * st) -> st
  val iter_dn : st -> t -> t -> (t -> st -> unit * st) -> st

  (* oo *)

  val dynmet : st -> t -> t -> t * st

  (* debugging *)

  val string_of_value : t -> string
end

module State_eval : State with type t = int64 and type st = Machine.state

type sp_cmd =
  | Get_reg of int * Machine.Register.t
  | Set_reg of Machine.Register.t * sp_t
  | Get_mem of int * Machine.Cache.t * sp_t
  | Set_mem of Machine.Cache.t * sp_t * sp_t
  | Cond of sp_t * sp_cmd list * sp_cmd list
  | Iter of bool * int * sp_t * sp_t * sp_cmd list

and sp_t =
  | Op of string * sp_t * sp_t
  | Val of int
  | Const of int

and sp_st =
  { id : int
  ; cmd : sp_cmd list
  }
[@@deriving sexp_of]

module State_poly : sig
  include State with type t = sp_t and type st = sp_st

  val empty : st
  val normalise : sp_cmd list -> sp_cmd list
  val print : st -> unit
end

module type Monad = sig
  module S : State

  type 'a t = S.st -> 'a * S.st

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val if_ : S.t -> unit t -> unit t -> unit t
  val for_up : S.t -> S.t -> (S.t -> unit t) -> unit t
  val for_dn : S.t -> S.t -> (S.t -> unit t) -> unit t
  val step : S.st -> 'a t -> 'a * S.st
  val trace : bool
  val debug : string -> unit t
  val write_reg : Machine.Register.t -> S.t -> unit t
  val read_reg : Machine.Register.t -> S.t t
  val modify_reg : Machine.Register.t -> (S.t -> S.t) -> unit t
  val read_mem : Machine.Cache.t -> S.t -> S.t t
  val write_mem : Machine.Cache.t -> S.t -> S.t -> unit t
  val read_bytecode : S.t -> S.t t
  val dynmet : S.t -> S.t -> S.t t
end

module Monad (T : sig
  val trace : bool
end)
(S : State) : Monad with type S.st = S.st and type S.t = S.t

module Opcodes (M : Monad) : sig
  type returns =
    [ `step
    | `stop
    | `c_call of M.S.t * M.S.t
    ]
  [@@deriving sexp_of]

  type instr = unit M.t
  type arg = M.S.t

  val accn : arg -> instr
  val acc : instr
  val push : instr
  val pushaccn : arg -> instr
  val pushacc : instr
  val pop : instr
  val assign : instr
  val envaccn : arg -> instr
  val envacc : instr
  val pushenvaccn : arg -> instr
  val pushenvacc : instr
  val push_retaddr : instr
  val apply : instr
  val applyn : int -> instr
  val appterm : instr
  val apptermn : int -> instr
  val return_ : instr
  val restart : instr
  val grab : instr
  val closure : instr
  val closurerec : instr
  val pushoffsetclosure : instr
  val offsetclosure : instr
  val pushoffsetclosurem2 : instr
  val offsetclosurem2 : instr
  val pushoffsetclosure0 : instr
  val offsetclosure0 : instr
  val pushoffsetclosure2 : instr
  val offsetclosure2 : instr
  val getglobal : instr
  val pushgetglobal : instr
  val getglobalfield : instr
  val pushgetglobalfield : instr
  val setglobal : instr
  val atom0 : instr
  val pushatom0 : instr
  val atom : instr
  val pushatom : instr
  val makeblockn : arg -> instr
  val makeblock : instr
  val makefloatblock : instr
  val getfieldn : arg -> instr
  val getfield : instr
  val getfloatfield : instr
  val setfieldn : arg -> instr
  val setfield : instr
  val vectlength : instr
  val getvectitem : instr
  val setvectitem : instr
  val getstringchar : instr
  val setstringchar : instr
  val branch : instr
  val branchif : instr
  val branchifnot : instr
  val switch : instr
  val boolnot : instr
  val pushtrap : instr
  val poptrap : instr
  val raise_ : instr
  val raise_notrace : instr
  val reraise : instr
  val check_signals : instr
  val c_call : arg -> returns M.t
  val c_calln : returns M.t
  val constn : arg -> instr
  val pushconstn : arg -> instr
  val constint : instr
  val pushconstint : instr
  val negint : instr
  val addint : instr
  val subint : instr
  val mulint : instr
  val divint : instr
  val modint : instr
  val andint : instr
  val orint : instr
  val xorint : instr
  val lslint : instr
  val lsrint : instr
  val asrint : instr
  val eq : instr
  val neq : instr
  val ltint : instr
  val leint : instr
  val gtint : instr
  val geint : instr
  val ultint : instr
  val ugeint : instr
  val beq : instr
  val bneq : instr
  val bltint : instr
  val bleint : instr
  val bgtint : instr
  val bgeint : instr
  val bultint : instr
  val bugeint : instr
  val offsetint : instr
  val offsetref : instr
  val isint : instr
  val getmethod : instr
  val getdynmet : instr
  val getpubmet : instr
  val stop : instr
  val event : instr
  val break : instr
  val dispatch : Opcode.t -> returns M.t
end
