(* Top level test design and testbench *)
open Base
open Hardcaml

module M =
  Interp.Monad
    (struct
      let trace = false
    end)
    (Interp.State_poly)

module O = Interp.Opcodes (M)
module Sequential = Compile_hardware.Sequential
module State_machine = Sequential.State_machine

let all_opcodes = Set.of_list (module Opcode) Opcode.all

(* XXX The following opcodes are not exposed properly in [Interp] yet. We don't
   (can't) compile them for now. *)
let not_implemented_opcodes =
  Set.of_list
    (module Opcode)
    [ RAISE; RERAISE; RAISE_NOTRACE; GETDYNMET; GETPUBMET; STOP; EVENT; BREAK ]
;;

let opcodes = Set.diff all_opcodes not_implemented_opcodes |> Set.to_list

let create (i : _ Sequential.I.t) =
  let reg_spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
  let state : State_machine.t = State_machine.initial_state ~reg_spec i in
  let state_machine_of_instruction opcode =
    let _, instruction = O.dispatch opcode Interp.State_poly.empty in
    try
      Sequential.State_machine.create ~instruction ~reg_spec ~enable:Signal.vdd state
    with
    | e ->
      raise_s [%message "failed to compile instruction" (opcode : Opcode.t) (e : exn)]
  in
  let state_machines = List.map opcodes ~f:state_machine_of_instruction in
  let done_ =
    List.map state_machines ~f:(fun (_, d) -> d.value) |> List.reduce_exn ~f:Signal.( |: )
  in
  let state_machines = List.map state_machines ~f:fst in
  Always.(
    compile
      (List.concat
         [ [ proc
               Sequential.Zinc_register.(
                 map state.zinc_registers ~f:(fun r -> r <-- r.value) |> to_list)
           ; proc
               Sequential.Memory_control.O.(
                 map state.memory_out ~f:(fun r ->
                     r <-- Signal.zero (Signal.width r.value))
                 |> to_list)
           ; proc
               Sequential.Memory_control.O.(
                 map state.stack_out ~f:(fun r ->
                     r <-- Signal.zero (Signal.width r.value))
                 |> to_list)
           ; proc
               Sequential.Memory_control.O.(
                 map state.program_out ~f:(fun r ->
                     r <-- Signal.zero (Signal.width r.value))
                 |> to_list)
           ]
         ; state_machines
         ]));
  let o =
    { Sequential.O.memory_out = state.memory_out
    ; program_out = state.program_out
    ; stack_out = state.stack_out
    ; zinc_registers = state.zinc_registers
    ; done_ = Always.Variable.wire ~default:Signal.gnd
    }
    |> Sequential.O.map ~f:(fun o -> o.value)
  in
  { o with Sequential.O.done_ }
;;
