open Hardcaml_zinc
open Expect_test_helpers_kernel

let%expect_test "int etc" =
  let zero = Repr.of_obj (Obj.repr 0) in
  let one = Repr.of_obj (Obj.repr 1) in
  let two = Repr.of_obj (Obj.repr 2) in
  print_s [%message (zero : Repr.t) (one : Repr.t) (two : Repr.t)];
  [%expect {|
    ((zero (Int 1))
     (one  (Int 3))
     (two  (Int 5))) |}];
  let none = Repr.of_obj (Obj.repr None) in
  print_s [%message (none : Repr.t)];
  [%expect {| (none (Int 1)) |}]
;;
