type state = 
  {
    env : int64;
    accu : int64;
    sp : int;
    memory : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  }

type c_call = 
  | C1 of (state -> int64 -> int64)
  | C2 of (state -> int64 -> int64 -> int64)
  | C3 of (state -> int64 -> int64 -> int64 -> int64)
  | C4 of (state -> int64 -> int64 -> int64 -> int64 -> int64)
  | C5 of (state -> int64 -> int64 -> int64 -> int64 -> int64 -> int64)
  | CN

val get_repr : 'a -> int -> int64 array

val alloc_block : state -> int64 -> int64 -> int64 -> int64
val alloc_block_from : state -> 'a -> int64 

val set_oo_id : c_call
val int64_float_of_bits : c_call
val open_descriptor : c_call
val out_channels_list : c_call
val output_char : c_call
val string_length : c_call
val output_string : c_call
val alloc_string : c_call

val init : int -> int -> unit

val run : Load.bytecode_exe -> int -> state -> int64

