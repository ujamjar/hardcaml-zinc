open Hardcaml_zinc
open Expect_test_helpers_kernel

let%expect_test "[Repr.of_obj] Int" =
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

let%expect_test "[Repr.of_obj] String" =
  let string = "hello world" |> Obj.repr |> Repr.of_obj in
  print_s [%message (string : Repr.t)];
  [%expect
    {| (string (Flat (header 2300) (data (0x6f77206f6c6c6568 0x400000000646c72)))) |}]
;;

let%expect_test "[Repr.of_obj] Record" =
  let module X = struct
    type t =
      { a : int
      ; b : t option
      }
  end
  in
  let record = { X.a = 20; b = None } |> Obj.repr |> Repr.of_obj in
  print_s [%message (record : Repr.t)];
  [%expect {|
    (record (
      Block
      (header 2048)
      (data (
        (Int 41)
        (Int 1))))) |}];
  let record = { X.a = 64; b = Some { a = 12; b = None } } |> Obj.repr |> Repr.of_obj in
  print_s [%message (record : Repr.t)];
  [%expect {|
    (record (
      Block
      (header 2048)
      (data (
        (Int 129)
        (Block
          (header 1024)
          (data ((
            Block
            (header 2048)
            (data (
              (Int 25)
              (Int 1))))))))))) |}]
;;
