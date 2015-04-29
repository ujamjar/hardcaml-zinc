type memory = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 

val init_memory : Load.bytecode_exe -> int -> 
  memory * int * int * int * int * int * int

type cfg = 
  {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

val trace_val : bytecode_address:int -> bytecode_size:int -> memory:memory -> int64 -> unit

val trace : 
  bytecode_address:int -> bytecode_size:int -> stack_address:int ->
  memory:memory -> env:int64 -> sp:int64 -> accu:int64 -> unit

val make : cfg -> Load.bytecode_exe -> unit


