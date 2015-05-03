type machine_register = 
  (* zinc registers *)
  [ `accu | `env | `pc | `sp | `extra_args | `trapsp
  (* other state *)
  | `global_data | `atom_table | `alloc_base | `stack_high ]

type cache = [ `stack | `program | `mem ]

type state = 
  {
    (* zinc registers *)
    accu : int64;
    env : int64;
    pc : int64;
    sp : int64;
    extra_args : int64;
    trapsp : int64;
    (* other state *)
    global_data : int64;
    atom_table : int64;
    alloc_base : int64;
    stack_high : int64;
    (* memory *)
    memory : Repr.memory;
    (* executable *)
    exe : Load.bytecode_exe;
  }

val empty : state
val string_of_mach_reg : machine_register -> string
val string_of_cache : cache -> string

