(* hardcaml-zinc byte code runtime *)

open Printf
open HardCamlZinc

let bytecode_filename = ref ""

let () = Arg.parse
  [
    "-i", Arg.Set_string bytecode_filename, " bytecode executable";
  ]
  (fun _ -> ())
  "hardcamlrun (c) 2015 MicroJamJar Ltd"

let bytecode = Load.bytecode_exe !bytecode_filename

(* show a few stats *)
let () = 
  let open Load in
  printf "toc:\n";
  List.iter (fun (n,l) -> printf "  %s = %i\n" n l) bytecode.toc;
  printf "code = %i\n" (Array.length bytecode.code);
  printf "prims = %i\n" (List.length bytecode.prim);
  printf "**********************************\n\n"

let _ = Framework.make bytecode

