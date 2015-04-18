(* build bytecodes by hand and debug the statemachine *)
open Printf
open HardCamlZinc

let mk_exe code = { Load.empty with Load.code = Instr.to_array code }

let run code = Framework.make
  Framework.{ waves=true; instr_trace=true; state_trace=true; mem_trace=true }
  (mk_exe code)

let () = 
  let open Instr in
  run [
    const3;
    push;
    const1;
    pushacc0;
    constint 25l;
    pushacc 0l;
    pushconst2;
    push;
    constint 139l;
    pushacc1;
    stop;
  ]

