open Base

val init_memory : Load.bytecode_exe -> int -> Machine.memory_mapping * Memory.t
val init_state : Machine.memory_mapping -> Memory.t -> Load.bytecode_exe -> Machine.state

module Interp : sig
  open Machine

  val init : prog:string -> argv:string array -> memsize_kb:int -> state
  val do_c_call : state -> Int64.t -> int64 -> state option
  val step : ?trace:int -> state -> state option

  val interactive
    :  prog:string
    -> argv:string array
    -> memsize_kb:int
    -> < step : unit
       ; steps : int -> unit
       ; stepto : int -> unit
       ; stepd : int
       ; stepsd : int -> int
       ; steptod : int -> int
       ; state : state
       ; running : bool
       ; ninstrs : int
       ; trace :
           < machine : unit
           ; instr : unit
           ; value : Int64.t -> unit
           ; root : Int64.t -> unit > >
end
