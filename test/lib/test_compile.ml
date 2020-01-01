open! Base
open Hardcaml
open Hardcaml_zinc
open! Expect_test_helpers_kernel
module Waveform = Hardcaml_waveterm.Waveform

let () = Caller_id.set_mode Full_trace

module M =
  Interp.Monad
    (struct
      let trace = false
    end)
    (Interp.State_poly)

module O = Interp.Opcodes (M)
module Sequential = Compile_hardware.Sequential
module Sim = Cyclesim.With_interface (Sequential.I) (Sequential.O)

let simulate_instruction instr =
  let _, instr = O.dispatch instr Interp.State_poly.empty in
  let sim = Sim.create ~is_internal_port:(Fn.const true) (Sequential.compile instr) in
  let i : _ Sequential.I.t = Cyclesim.inputs sim in
  let waves, sim = Waveform.create sim in
  let set x i = x := Bits.consti ~width:(Bits.width !x) i in
  i.memory_in.read_available := Bits.vdd;
  set i.memory_in.read_data 10;
  i.program_in.read_available := Bits.vdd;
  set i.program_in.read_data 20;
  i.stack_in.read_available := Bits.vdd;
  set i.stack_in.read_data 30;
  i.memory_in.write_complete := Bits.vdd;
  i.program_in.write_complete := Bits.vdd;
  i.stack_in.write_complete := Bits.vdd;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  let display_rules, display_height = Instruction_display_rules.create instr.cmd in
  Waveform.print waves ~display_rules ~display_height ~display_width:90
;;

let%expect_test "ACC0" =
  simulate_instruction ACC0;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │                  ││────────┬───────┬───────┬───────────────────────────────────────────│
    │state             ││ 0      │1      │2      │3                                          │
    │                  ││────────┴───────┴───────┴───────────────────────────────────────────│
    │                  ││────────────────────────┬───────────────────────────────────────────│
    │Accu              ││ 0                      │30                                         │
    │                  ││────────────────────────┴───────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │Sp                ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_read           ││        ┌───────┐                                                   │
    │                  ││────────┘       └───────────────────────────────────────────────────│
    │si_read_available ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │si_read_data      ││ 30                                                                 │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_read_address   ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "ACC1" =
  simulate_instruction ACC1;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │                  ││────────┬───────┬───────┬───────────────────────────────────────────│
    │state             ││ 0      │1      │2      │3                                          │
    │                  ││────────┴───────┴───────┴───────────────────────────────────────────│
    │                  ││────────────────────────┬───────────────────────────────────────────│
    │Accu              ││ 0                      │30                                         │
    │                  ││────────────────────────┴───────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │Sp                ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_read           ││        ┌───────┐                                                   │
    │                  ││────────┘       └───────────────────────────────────────────────────│
    │si_read_available ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │si_read_data      ││ 30                                                                 │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───────┬───────────────────────────────────────────────────│
    │so_read_address   ││ 0      │1      │0                                                  │
    │                  ││────────┴───────┴───────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "ACC" =
  simulate_instruction ACC;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────│
    │state             ││ 0      │1      │2      │3      │4      │5      │6      │7          │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────│
    │                  ││────────────────────────────────────────────────────────┬───────────│
    │Accu              ││ 0                                                      │30         │
    │                  ││────────────────────────────────────────────────────────┴───────────│
    │                  ││────────────────────────────────┬───────────────────────────────────│
    │Pc                ││ 0                              │4                                  │
    │                  ││────────────────────────────────┴───────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │Sp                ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │po_read           ││        ┌───────┐                                                   │
    │                  ││────────┘       └───────────────────────────────────────────────────│
    │pi_read_available ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │pi_read_data      ││ 20                                                                 │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │po_read_address   ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_read           ││                                        ┌───────┐                   │
    │                  ││────────────────────────────────────────┘       └───────────────────│
    │si_read_available ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │si_read_data      ││ 30                                                                 │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_read_address   ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "PUSH" =
  simulate_instruction PUSH;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │                  ││────────┬───────┬───────┬───────┬───────┬───────────────────────────│
    │state             ││ 0      │1      │2      │3      │4      │5                          │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────│
    │Accu              ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬───────────────────────────────────────────│
    │Sp                ││ 0                      │18446744073709551608                       │
    │                  ││────────────────────────┴───────────────────────────────────────────│
    │so_write          ││                                ┌───────┐                           │
    │                  ││────────────────────────────────┘       └───────────────────────────│
    │si_write_complete ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │so_write_data     ││ 0                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────┬───────┬───────────────────────────│
    │so_write_address  ││ 0                              │230584.│0                          │
    │                  ││────────────────────────────────┴───────┴───────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
