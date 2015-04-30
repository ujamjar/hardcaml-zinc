(* test the monadic interpreter *)

let bytecode_filename = ref ""
let memsize_kb = ref 1024
let num_instrs = ref (-1)
let trace = ref 0

let () = Arg.parse
  [
    "-i", Arg.Set_string bytecode_filename, " bytecode executable";
    "-m", Arg.Set_int memsize_kb, " memory size in Kb";
    "-c", Arg.Set_int num_instrs, " number of instructions to run";
    "-t", Arg.Unit (fun () -> incr trace), " increase trace level";
  ]
  (fun _ -> ())
  "monadinterp (c) 2015 MicroJamJar Ltd"

let bytecode = Load.bytecode_exe !bytecode_filename

let mm = Framework.init_memory bytecode (!memsize_kb * (1024/8))

module S = Interp.State_eval
module type M_eval = Interp.Monad 
    with type S.t = int64
     and type S.st = S.st
module M : M_eval = Interp.Monad(struct let trace = !trace>2 end)(S) 
module O = Interp.Opcodes(M)

let state = 
  S.({
    initial() with
      pc = Int64.of_int mm.Framework.code_address;
      sp = Int64.of_int mm.Framework.stack_address;
      env = Int64.of_int (mm.Framework.atoms_address + 8);
      trapsp = Int64.of_int mm.Framework.stack_address;
      global_data = Int64.of_int (mm.Framework.globals_address + 8);
      atom_table = Int64.of_int (mm.Framework.atoms_address + 8);
      alloc_base = Int64.of_int mm.Framework.heap_address;
      memory = mm.Framework.memory;
      exe = bytecode;
  })

let step n st = 
  let open S in
  let open Printf in
  (* fetch instruction *)
  let pc = Int64.to_int st.pc / 4 in
  let instr = mm.Framework.memory.{pc / 2} in
  let instr = S.(srl (if pc mod 2 = 0 then sll instr 32L else instr) 32L) in
  let st = { st with pc = S.(st.pc +: 4L) } in
  let instr = Enum.to_enum<Instr.opcodes> (Int64.to_int instr) in
  let () = 
    let bytecode_size = (Array.length bytecode.Load.code * 4) in
    (if !trace>1 then printf "\n##%i\n" (n+1));
    (if !trace>0 then printf "%6i  %s\n" pc (Show.show<Instr.opcodes> instr));
    (if !trace>1 then
      Framework.trace ~m:mm ~env:st.env ~sp:st.sp ~accu:st.accu)
  in
  (* execute instruction *)
  if instr = Instr.STOP then failwith "STOP!"
  else
    M.step st O.(dispatch instr)

let rec run n st = 
  if n = !num_instrs then ()
  else run (n+1) (step n st)

let () = run 0 state


