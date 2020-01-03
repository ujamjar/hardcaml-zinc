open Hardcaml
module Sequential = Compile_hardware.Sequential

val create : Signal.t Hardcaml.Interface.Create_fn(Sequential.I)(Sequential.O).t
