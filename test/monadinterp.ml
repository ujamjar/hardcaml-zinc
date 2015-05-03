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

let mm = Framework.init_memory bytecode (!memsize_kb * (1024/8))

open Machine

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
      stack_high = Int64.of_int mm.Framework.stack_address;
      memory = mm.Framework.memory;
      exe = bytecode;
  })

let is_c_call i =
  let open Instr in
  match i with
  | C_CALL1 | C_CALL2 | C_CALL3 | C_CALL4 | C_CALL5 | C_CALLN -> true
  | _ -> false

let do_c_call st nargs prim = 
  let open Ops.Int64 in
  let open Int64 in
  assert (nargs <> 0L); (* XXX C_CALLN TODO *)
  let prim = to_int prim in
  let setup_for_c_call st = 
    let st = { st with sp = st.sp -: 8L } in
    st.memory.{to_int st.sp / 8} <- st.env;
    st
  in
  let restore_after_c_call st v = 
    { st with 
      env = st.memory.{to_int st.sp / 8}; 
      sp = st.sp +: (nargs *: 8L);
      accu = v } 
  in
  let do_exception st v = 
    let _, st = M.step { st with accu=v } O.(dispatch Instr.RAISE) in
    Some st
  in
  let st = setup_for_c_call st in
  match C_runtime.run bytecode prim st with
  | `ok v -> Some( restore_after_c_call st v )
  | `exn v -> do_exception st v

let rec step n st = 
  let open S in
  (* fetch instruction *)
  let pc = Int64.to_int st.pc / 4 in
  let get_instr pc = 
    let instr = mm.Framework.memory.{pc / 2} in
    S.(srl (if pc mod 2 = 0 then sll instr 32L else instr) 32L) 
  in
  let instr = get_instr pc in
  let st = { st with pc = S.(st.pc +: 4L) } in
  let instr = Enum.to_enum<Instr.opcodes> (Int64.to_int instr) in
  let () = 
    (if !trace>1 then printf "\n##%i\n" (n+1));
    (if !trace>0 then begin
      if is_c_call instr then
        let prim = Int64.to_int @@ get_instr (Int64.to_int st.pc / 4) in
        printf "%6i  %s %s\n" pc (Show.show<Instr.opcodes> instr) state.exe.Load.prim.(prim)
      else
        printf "%6i  %s\n" pc (Show.show<Instr.opcodes> instr)
    end);
    (if !trace>1 then
      Framework.trace ~m:mm ~env:st.env ~sp:st.sp ~accu:st.accu 
        ~trapsp:st.trapsp ~eargs:st.extra_args)
  in
  (* execute instruction *)
  let result, st = M.step st O.(dispatch instr) in
  match result with
  | `step -> Some(st)
  | `stop -> None
  | `c_call(nargs,prim)-> do_c_call st nargs prim

let rec run n st = 
  if n = !num_instrs then ()
  else 
    match step n st with
    | Some(st) -> run (n+1) st
    | None -> ()

let () = run 0 state


