module Memory : sig
  module I : sig
    type 'a t = {
      (* memory data input *)
      memory_data_in : 'a;
      (* memory data in ready *)
      memory_ready : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = {
      (* memory access request *)
      memory_request : 'a;
      (* memory read/write *)
      memory_read_write : 'a;
      (* memory address *)
      memory_address : 'a;
      (* memory data out *)
      memory_data_out : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end
end

open Hardcaml
open Signal

val memory_if :
  e:t ->
  stack_o:Always.Variable.t Memory.O.t ->
  bc_o:Always.Variable.t Memory.O.t ->
  mem_o:Always.Variable.t Memory.O.t ->
  ext_i:t Memory.I.t ->
  t Memory.O.t * t Memory.I.t * t Memory.I.t * t Memory.I.t

module Decode : Interface.S

module I : sig
  type 'a t = {
    start : 'a;
    bytecode_start_address : 'a;
    atom_table_address : 'a;
    globals_start_address : 'a;
    heap_start_address : 'a;
    stack_start_address : 'a;
    memory_i : 'a Memory.I.t;
    c_call_ready : 'a;
    c_call_result : 'a;
  }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = {
    state : 'a;
    pc : 'a;
    sp : 'a;
    accu : 'a;
    env : 'a;
    extra_args : 'a;
    instruction : 'a;
    error : 'a;
    memory_o : 'a Memory.O.t;
    decode : 'a Decode.t;
    c_call_request : 'a;
    c_call_prim : 'a;
  }
  [@@deriving sexp_of, hardcaml]
end

val state_str : string list

val zinc : t I.t -> t O.t
