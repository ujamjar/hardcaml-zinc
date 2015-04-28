
val init_memory : Load.bytecode_exe -> int ->
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * 
  int * int * int * int * int * int

type cfg = 
  {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

val make : cfg -> Load.bytecode_exe -> unit


