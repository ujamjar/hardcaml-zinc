open Base
open Machine

type result = (Int64.t, Int64.t) Result.t

type c_call =
  | C1 of (state -> Int64.t -> result)
  | C2 of (state -> Int64.t -> Int64.t -> result)
  | C3 of (state -> Int64.t -> Int64.t -> Int64.t -> result)
  | C4 of (state -> Int64.t -> Int64.t -> Int64.t -> Int64.t -> result)
  | C5 of (state -> Int64.t -> Int64.t -> Int64.t -> Int64.t -> Int64.t -> result)
  | CN

exception Get_repr
exception Get_obj
exception Alloc_block_from

val get_repr : ?closure:bool -> 'a -> int -> Int64.t array
val get_obj : ?closure:bool -> state -> Int64.t -> 'a
val alloc_block : st:state -> size:Int64.t -> colour:Int64.t -> tag:Int64.t -> Int64.t
val alloc_block_from : state -> 'a -> Int64.t
val init : int -> int -> unit
val argv : (string * string array) ref
val run : Load.bytecode_exe -> int -> state -> result
