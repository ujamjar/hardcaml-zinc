open Core
open! Hardcaml_zinc

module Common = struct
  type args =
    { bytecode_file : string
    ; num_instructions : int
    ; mem_size_kb : int
    ; argv : string * string array
    }
  [@@deriving sexp_of]

  let args =
    let open Command.Let_syntax in
    let%map_open bytecode_file = anon ("BYTECODE" %: string)
    and num_instructions = flag "-instructions" (optional_with_default (-1) int) ~doc:""
    and mem_size_kb = flag "-mem-size" (optional_with_default 16384 int) ~doc:""
    and argv = flag "--" escape ~doc:"" in
    let argv =
      bytecode_file, Array.of_list (bytecode_file :: Option.value ~default:[] argv)
    in
    { bytecode_file; num_instructions; mem_size_kb; argv }
  ;;

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
end

module Software = struct
  let run ~trace (args : Common.args) =
    let runtime = Common.create_runtime args in
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

  let command =
    Command.basic
      ~summary:""
      (let open Command.Let_syntax in
      let%map_open args = Common.args
      and trace = flag "-t" (optional_with_default 0 int) ~doc:"" in
      fun () -> run ~trace args)
  ;;
end

module Hardware = struct
  let run (args : Common.args) = print_s [%message (args : Common.args)]

  let command =
    Command.basic
      ~summary:""
      (let open Command.Let_syntax in
      let%map_open args = Common.args in
      fun () -> run args)
  ;;
end

let () =
  Command.run
    (Command.group
       ~summary:""
       [ "software", Software.command; "hardware", Hardware.command ])
;;
