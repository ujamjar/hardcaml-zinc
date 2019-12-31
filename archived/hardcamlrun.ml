(* hardcaml-zinc byte code runtime *)

open Printf
open HardCamlZinc

let bytecode_filename = ref ""
let waves = ref false
let instr_trace = ref false
let state_trace = ref false
let mem_trace = ref false

let () =
  Arg.parse
    [ "-i", Arg.Set_string bytecode_filename, " bytecode executable"
    ; "-waves", Arg.Set waves, " waveform viewer"
    ; "-t", Arg.Set instr_trace, " instruction trace"
    ; "-tt", Arg.Set state_trace, " detailed trace"
    ; "-ttt", Arg.Set mem_trace, " memory trace"
    ]
    (fun _ -> ())
    "hardcamlrun (c) 2015 MicroJamJar Ltd"
;;

let bytecode = Load.bytecode_exe !bytecode_filename

(* show a few stats *)
let () =
  let open Load in
  if !state_trace
  then (
    printf "toc:\n";
    List.iter (fun (n, l) -> printf "  %s = %i\n" n l) bytecode.toc;
    printf "code = %i\n" (Array.length bytecode.code);
    printf "prims = %i\n" (Array.length bytecode.prim);
    Array.iteri (Printf.printf "  [%i] %s\n") bytecode.prim;
    printf "**********************************\n\n")
;;

let _ =
  let cfg =
    Framework.Testbench.
      { waves = !waves
      ; instr_trace = !instr_trace
      ; state_trace = !state_trace
      ; mem_trace = !mem_trace
      }
  in
  Framework.Testbench.make cfg bytecode
;;
