open Base
open Hardcaml_zinc
open Expect_test_helpers_kernel
module Obj = Caml.Obj

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

type x =
  { a : int
  ; b : x option
  }
[@@deriving sexp_of]

let%expect_test "[Repr.of_obj] Record" =
  let record = { a = 20; b = None } |> Obj.repr |> Repr.of_obj in
  print_s [%message (record : Repr.t)];
  [%expect
    {|
    (record (
      Block
      (header 2048)
      (data (
        (Int 41)
        (Int 1))))) |}];
  let record = { a = 64; b = Some { a = 12; b = None } } |> Obj.repr |> Repr.of_obj in
  print_s [%message (record : Repr.t)];
  [%expect
    {|
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

let roundtrip x = x |> Obj.repr |> Repr.of_obj |> Repr.to_obj |> Obj.magic

let%expect_test "Rountrip via [Repr.t], int" =
  let zero = roundtrip 0 in
  let one = roundtrip 1 in
  let two = roundtrip 2 in
  print_s [%message (zero : int) (one : int) (two : int)];
  [%expect {|
    ((zero 0)
     (one  1)
     (two  2)) |}]
;;

let%expect_test "Roundtrip via [Repr.t], string" =
  let string = roundtrip "hello world" in
  print_s [%message (string : string)];
  [%expect {| (string "hello world") |}]
;;

let%expect_test "Roundtrip via [Repr.t], record " =
  let record = roundtrip { a = 20; b = None } in
  print_s [%message (record : x)];
  [%expect
    {|
    (record ((a 20) (b ()))) |}];
  let record = roundtrip { a = 64; b = Some { a = 12; b = None } } in
  print_s [%message (record : x)];
  [%expect {| (record ((a 64) (b (((a 12) (b ())))))) |}]
;;
