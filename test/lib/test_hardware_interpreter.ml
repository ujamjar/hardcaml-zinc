open Hardcaml
open Hardcaml_zinc
module Sequential = Compile_hardware.Sequential
module Sim = Cyclesim.With_interface (Sequential.I) (Sequential.O)

let%expect_test "elaborates" = ignore (Sim.create Hardware_interpreter.create : Sim.t)
