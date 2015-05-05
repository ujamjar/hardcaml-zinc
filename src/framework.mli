val init_memory : Load.bytecode_exe -> int -> Machine.memory_mapping

type cfg = 
  {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

val make : cfg -> Load.bytecode_exe -> unit


