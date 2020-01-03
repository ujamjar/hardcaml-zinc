open Hardcaml
open Hardcaml_zinc
module Sequential = Compile_hardware.Sequential
module Circuit = Circuit.With_interface (Sequential.I) (Sequential.O)

let () = Rtl.print Verilog (Circuit.create_exn ~name:"zinc" Hardware_interpreter.create)
