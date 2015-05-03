type memory_mapping = 
  {
    memory : Repr.memory;
    code_address : int;
    code_size : int;
    atoms_address : int;
    globals_address : int;
    c_heap_address : int;
    c_heap_size : int;
    heap_address : int;
    stack_address : int;
  }

val init_memory : Load.bytecode_exe -> int -> memory_mapping

type cfg = 
  {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

val trace_val : memory_mapping -> int64 -> unit

val trace : 
  m:memory_mapping -> env:int64 -> sp:int64 -> accu:int64 -> 
  trapsp:int64 -> eargs:int64 -> unit

val make : cfg -> Load.bytecode_exe -> unit


