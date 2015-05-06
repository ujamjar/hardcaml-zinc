(* test the monadic interpreter *)
open Printf

let bytecode_filename = ref ""
let argv = ref []
let memsize_kb = ref 1024
let num_instrs = ref (-1)
let trace = ref 0

(*
let () = Arg.parse
  [
    "-i", Arg.Set_string bytecode_filename, " bytecode executable";
    "-m", Arg.Set_int memsize_kb, " memory size in Kb";
    "-c", Arg.Set_int num_instrs, " number of instructions to run";
    "-t", Arg.Unit (fun () -> incr trace), " increase trace level";
  ]
  (fun _ -> ())
  "monadinterp (c) 2015 MicroJamJar Ltd"
*)

let () = 
  let run_args = ref [] in
  let rec anon_fun s = 
    if !bytecode_filename = "" then begin
      bytecode_filename := s;
      run_args := [ "-", Arg.String anon_fun, " arguments passed to inferior" ]
    end else begin
      argv := s :: !argv
    end
  in
  run_args := [
    "-m", Arg.Set_int memsize_kb, " memory size in Kb";
    "-c", Arg.Set_int num_instrs, " number of instructions to run";
    "-t", Arg.Unit (fun () -> incr trace), " increase trace level";
  ];
  Arg.parse_dynamic run_args anon_fun
    "monadinterp (c) 2015 MicroJamJar Ltd"

let () = if !bytecode_filename = "" then failwith "No bytecode file specified"

let () = C_runtime.argv := 
  (!bytecode_filename, Array.of_list (!bytecode_filename :: List.rev !argv))

let bytecode = Load.bytecode_exe !bytecode_filename

let mapping, memory = Framework.init_memory bytecode (!memsize_kb * (1024/8))
let state = Framework.init_state mapping memory bytecode

let () = if !trace>2 then Trace.showfields := true

let rec run n st = 
  if n = !num_instrs then ()
  else 
    let () = if !trace>1 then printf "\n##%i\n" (n+1) in
    match Framework.Interp.step ~trace:!trace st with
    | Some(st) -> run (n+1) st
    | None -> ()

let () = run 0 state


