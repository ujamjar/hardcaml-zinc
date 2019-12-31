open Base

val showfields : bool ref
val value : ?chan:Stdio.Out_channel.t -> Machine.state -> Int64.t -> unit
val machine : ?chan:Stdio.Out_channel.t -> Machine.state -> unit
val root : ?chan:Stdio.Out_channel.t -> Machine.state -> Int64.t -> unit
val instr : ?chan:Stdio.Out_channel.t -> Machine.state -> unit
