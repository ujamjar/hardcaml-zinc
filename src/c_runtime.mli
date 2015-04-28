type state = 
  {
    env : int64;
    accu : int64;
    sp : int;
    memory : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  }

val init : int -> unit

val run : Load.bytecode_exe -> int -> state -> int64

