(* print all the instructions *)

#require "compiler-libs.bytecomp,hardcaml-examples,deriving";;
#directory "_build/src";;
#load "HardCamlZinc.cma";;

open HardCamlZinc
open Zinc2

let show instr = 
  try
    S.print @@ Compile.simplify @@ snd @@ O.dispatch instr S.empty
  with _ -> 
    Printf.printf "NOT IMPLEMENTED\n"

let () =
  let range = Instr.(Enum_opcodes.enum_from_to 
    Bounded_opcodes.min_bound Bounded_opcodes.max_bound)
  in
  List.iter 
    (fun instr -> 
      Printf.printf "____ %s ____\n\n" (Instr.Show_opcodes.show instr);
      show instr;
      Printf.printf "\n") 
    range

