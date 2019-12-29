open! Base
open Hardcaml_zinc

module M =
  Interp.Monad
    (struct
      let trace = false
    end)
    (Interp.State_poly)

module O = Interp.Opcodes (M)

let%expect_test "ACC0" =
  let _, s = O.dispatch ACC0 Interp.State_poly.empty in
  Compile_hardware.Statement.simplify s |> Compile_hardware.compile
;;
