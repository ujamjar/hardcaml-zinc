open Machine

type result = [ `ok of int64 | `exn of int64 ]

type c_call = 
  | C1 of (state -> int64 -> result)
  | C2 of (state -> int64 -> int64 -> result)
  | C3 of (state -> int64 -> int64 -> int64 -> result)
  | C4 of (state -> int64 -> int64 -> int64 -> int64 -> result)
  | C5 of (state -> int64 -> int64 -> int64 -> int64 -> int64 -> result)
  | CN

val get_repr : 'a -> int -> int64 array
val get_obj : state -> int64 -> 'a

val alloc_block : st:state -> size:int64 -> colour:int64 -> tag:int64 -> int64
val alloc_block_from : state -> 'a -> int64 

val init : int -> int -> unit

val argv : (string * string array) ref

val run : Load.bytecode_exe -> int -> state -> result

