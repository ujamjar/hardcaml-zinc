open Base

module Register : sig
  (* zinc registers *)
  type t =
    | Accu
    | Env
    | Pc
    | Sp
    | Extra_args
    | Trapsp
    | Global_data
    | Atom_table
    | Alloc_base
    | Stack_high
  [@@deriving equal, compare, sexp_of, enumerate, variants]

  include Comparator.S with type t := t
end

module Cache : sig
  type t =
    | Stack
    | Program
    | Mem
  [@@deriving equal, compare, sexp_of, enumerate, variants]

  include Comparator.S with type t := t
end

type memory_mapping =
  { code_address : int
  ; code_size : int
  ; atoms_address : int
  ; globals_address : int
  ; c_heap_address : int
  ; c_heap_size : int
  ; heap_address : int
  ; stack_address : int
  }

type state =
  { (* zinc registers *)
    accu : int64
  ; env : int64
  ; pc : int64
  ; sp : int64
  ; extra_args : int64
  ; trapsp : int64
  ; (* other state *)
    global_data : int64
  ; atom_table : int64
  ; alloc_base : int64
  ; stack_high : int64
  ; (* memory *)
    memory : Memory.t
  ; (* executable *)
    exe : Load.bytecode_exe
  ; mapping : memory_mapping
  }

val empty : state
val string_of_mach_reg : Register.t -> string
val string_of_cache : Cache.t -> string
val num_machine_registers : int
val num_cache_spaces : int
