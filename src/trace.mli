val showfields : bool ref
val value : ?chan:out_channel -> Machine.state -> int64 -> unit
val machine : ?chan:out_channel -> Machine.state -> unit
val root : ?chan:out_channel -> Machine.state -> int64 -> unit
val instr : ?chan:out_channel -> Machine.state -> unit
