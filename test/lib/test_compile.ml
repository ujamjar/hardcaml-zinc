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

let display_rules =
  Hardcaml_waveterm.Display_rules.(
    Rule.
      [ Sequential.I.(
          map port_names ~f:(fun n -> port_name_is n ~wave_format:(Bit_or Hex)) |> to_list)
      ; Sequential.O.(
          map port_names ~f:(fun n -> port_name_is n ~wave_format:(Bit_or Hex)) |> to_list)
      ; [ port_name_is "state" ~wave_format:Unsigned_int ]
      ]
    |> List.concat
    |> of_list)
;;

let%expect_test "ACC0" =
  let _, instr = O.dispatch ACC0 Interp.State_poly.empty in
  let sim = Sim.create ~is_internal_port:(Fn.const true) (Sequential.compile instr) in
  let i : _ Sequential.I.t = Cyclesim.inputs sim in
  let waves, sim = Waveform.create sim in
  let set x i = x := Bits.consti ~width:(Bits.width !x) i in
  i.memory_in.read_available := Bits.vdd;
  set i.memory_in.read_data 0xdeadbeef;
  i.program_in.read_available := Bits.vdd;
  set i.program_in.read_data 0xdeadbeef;
  i.stack_in.read_available := Bits.vdd;
  set i.stack_in.read_data 0xdeadbeef;
  i.memory_in.write_complete := Bits.vdd;
  i.program_in.write_complete := Bits.vdd;
  i.stack_in.write_complete := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print waves ~display_rules ~display_height:90 ~display_width:90;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │clear             ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │si_read_available ││────────────────────────────────────────                            │
    │                  ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │si_read_data      ││ 00000000DEADBEEF                                                   │
    │                  ││────────────────────────────────────────                            │
    │mo_read           ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │mo_read_address   ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │mo_write          ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │mo_write_data     ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │mo_write_address  ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │po_read           ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │po_read_address   ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │po_write          ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │po_write_data     ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │po_write_address  ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │so_read           ││        ┌───────┐                                                   │
    │                  ││────────┘       └───────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │so_read_address   ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │so_write          ││                                                                    │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │so_write_data     ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │so_write_address  ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────┬───────────────                            │
    │Accu              ││ 0000000000000000       │00000000DEADBE.                            │
    │                  ││────────────────────────┴───────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Env               ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Pc                ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Sp                ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Extra_args        ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Trapsp            ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Global_data       ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Atom_table        ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Alloc_base        ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────────────────────────────────────                            │
    │Stack_high        ││ 0000000000000000                                                   │
    │                  ││────────────────────────────────────────                            │
    │                  ││────────┬───────┬───────┬───────────────                            │
    │state             ││ 0      │1      │2      │3                                          │
    │                  ││────────┴───────┴───────┴───────────────                            │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
