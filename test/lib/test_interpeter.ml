open Base
open Stdio
open Hardcaml_zinc

module Software_interpreter = struct
  type args =
    { bytecode_file : string
    ; num_instructions : int
    ; mem_size_kb : int
    ; argv : string * string array
    }
  [@@deriving sexp_of]

  type runtime =
    { bytecode : Load.bytecode_exe
    ; mapping : Machine.memory_mapping
    ; memory : Memory.t
    ; state : Machine.state
    }

  let create_runtime (args : args) =
    C_runtime.argv := args.argv;
    let bytecode = Load.bytecode_exe args.bytecode_file in
    let mapping, memory =
      Framework.init_memory bytecode (args.mem_size_kb * (1024 / 8))
    in
    let state = Framework.init_state mapping memory bytecode in
    { bytecode; mapping; memory; state }
  ;;

  let run ~trace (args : args) =
    let runtime = create_runtime args in
    if trace > 2 then Trace.showfields := true;
    let rec run n st =
      if n = args.num_instructions
      then ()
      else (
        let () = if trace > 1 then printf "\n##%i\n" (n + 1) in
        match Framework.Interp.step ~trace st with
        | Some st -> run (n + 1) st
        | None -> ())
    in
    run 0 runtime.state
  ;;
end

let run ?(trace = 0) ?(num_instructions = -1) ?(mem_size_kb = 16384) ~bytecode_file args =
  Software_interpreter.(
    run
      ~trace
      { bytecode_file
      ; num_instructions
      ; mem_size_kb
      ; argv = bytecode_file, bytecode_file :: args |> Array.of_list
      })
;;

let%expect_test "hello world" =
  run ~bytecode_file:"../examples/helloworld.bc" [];
  [%expect {| Hello world! |}]
;;

let%expect_test "runtime" =
  run ~bytecode_file:"../examples/runtime.bc" [];
  [%expect {|
    backend_type bytecode
    word_size 64
    int_size 63 |}]
;;

(* let%expect_test "arithmetic" = run ~bytecode_file:"../examples/arithmetic.bc" [] *)
