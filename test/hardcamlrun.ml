(* hardcaml-zinc byte code runtime *)

open HardCamlZinc

let bytecode_filename = ref ""

let () = Arg.parse
  [
    "-i", Arg.Set_string bytecode_filename, " bytecode executable";
  ]
  (fun _ -> ())
  "hardcamlzinc (c) 2015 MicroJamJar Ltd"


