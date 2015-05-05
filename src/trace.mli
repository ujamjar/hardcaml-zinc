val showfields : bool ref

val value : Machine.memory_mapping -> int -> int64 -> unit

val machine : 
  m:Machine.memory_mapping -> env:int64 -> sp:int64 -> accu:int64 -> 
  trapsp:int64 -> eargs:int64 -> unit

val root : out_channel -> Repr.memory -> int64 -> unit


