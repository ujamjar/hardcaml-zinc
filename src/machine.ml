type machine_register =
  (* zinc registers *)
  [ `accu
  | `env
  | `pc
  | `sp
  | `extra_args
  | `trapsp
  | (* other state *)
    `global_data
  | `atom_table
  | `alloc_base
  | `stack_high
  ]
[@@deriving equal, compare, sexp_of, enumerate, variants]

type cache =
  [ `stack
  | `program
  | `mem
  ]
[@@deriving equal, compare, sexp_of, enumerate, variants]

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
    memory : Repr.memory
  ; (* executable *)
    exe : Load.bytecode_exe
  ; mapping : memory_mapping
  }

let empty =
  let memory = Bigarray.(Array1.create int64 c_layout 0) in
  { accu = 0L
  ; env = 0L
  ; pc = 0L
  ; sp = 0L
  ; extra_args = 0L
  ; trapsp = 0L
  ; global_data = 0L
  ; atom_table = 0L
  ; alloc_base = 0L
  ; stack_high = 0L
  ; memory
  ; exe = Load.empty
  ; mapping =
      { code_address = 0
      ; code_size = 0
      ; atoms_address = 0
      ; globals_address = 0
      ; c_heap_address = 0
      ; c_heap_size = 0
      ; heap_address = 0
      ; stack_address = 0
      }
  }
;;

let string_of_mach_reg = function
  | `accu -> "accu"
  | `env -> "env"
  | `pc -> "pc"
  | `sp -> "sp"
  | `extra_args -> "extra_args"
  | `trapsp -> "trapsp"
  | `global_data -> "global_data"
  | `atom_table -> "atom_table"
  | `alloc_base -> "alloc_base"
  | `stack_high -> "stack_high"
;;

let string_of_cache = function
  | `stack -> "stack"
  | `program -> "program"
  | `mem -> "mem"
;;

let num_machine_registers = List.length all_of_machine_register
let num_cache_spaces = List.length all_of_cache
