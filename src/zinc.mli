module Memory : sig
  module I : interface
    (* memory data input *)
    memory_data_in
    (* memory data in ready *)
    memory_ready
  end
  module O : interface
    (* memory access request *)
    memory_request
    (* memory read/write *)
    memory_read_write
    (* memory address *)
    memory_address
    (* memory data out *)
    memory_data_out
  end
end

open HardCaml.Signal
open Comb

val memory_if : 
  e:t -> 
  stack_o:Guarded.variable Memory.O.t -> 
  bc_o:Guarded.variable Memory.O.t -> 
  mem_o:Guarded.variable Memory.O.t ->
  ext_i:t Memory.I.t ->
  t Memory.O.t * 
  t Memory.I.t * t Memory.I.t * t Memory.I.t  

module Decode : HardCaml.Interface.S

module I : interface
  start
  bytecode_start_address
  atom_table_address
  globals_start_address
  heap_start_address
  stack_start_address
  (memory_i : Memory.I)
  c_call_ready
  c_call_result
end

module O : interface
  state
  pc
  sp
  accu
  env
  extra_args
  instruction
  error
  (memory_o : Memory.O)
  (decode : Decode)
  c_call_request
  c_call_prim
end

val state_str : string list

val zinc : t I.t -> t O.t

