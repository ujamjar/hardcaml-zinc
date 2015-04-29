open Printf
open HardCaml
module B = Bits_ext.Comb.ArraybitsInt64
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

module Z = Interface.Gen(B)(Zinc.I)(Zinc.O)
(*module Z = HardCamlLlvmsim.Sim.Gen(B)(Zinc.I)(Zinc.O)*)
module If = Cyclesim.Sim_obj_if.Make(B)

type memory = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 

let init_memory bc memory_size_words = 
  let open Int64 in
  let open Load in
  (* convert exe to 64 bit *)
  let code_size = (Array.length bc.code + 1) / 2 in
  let code_address = 0 in
  let exe i = 
    let a, b = of_int32 bc.code.(i*2), try of_int32 bc.code.(i*2+1) with _ -> 0L in
    let a, b = logand a 0xFFFFFFFFL, logand b 0xFFFFFFFFL in
    logor a (shift_left b 32)
  in
  (* atoms table *)
  let atom i = of_int i in
  let atoms_address = code_size in
  (* globals *)
  let globals_address = atoms_address + 256 in
  let globals = get_global_data64 bc globals_address in
  (* init data *)
  let c_heap_address = globals_address + (Array.length globals) in
  let heap_address = c_heap_address + 1024 in (* XXX *)
  let memory = Bigarray.(Array1.create int64 c_layout memory_size_words) in
  let () = 
    for i=0 to memory_size_words-1 do
      memory.{i} <- 
        if i<code_size then exe i
        else if i<globals_address then atom (i-code_size)
        else if i<c_heap_address then globals.(i-globals_address)
        else 0L
    done
  in
  let stack_address = memory_size_words in
  memory, code_address*8, atoms_address*8, globals_address*8, 
  c_heap_address*8, heap_address*8, stack_address*8

type cfg = 
  {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

module M = Mlvalues.Make(Ops.Int64) 

let header memory v = memory.{((Int64.to_int v) / 8)-1} 
let field memory v i = memory.{((Int64.to_int v) / 8)+i} 

let get_string memory v = 
  let open Int64 in
  let size = to_int @@ M.size (header memory v) in
  let pad = to_int @@ shift_right_logical (field memory v (size-1)) 56 in
  String.init ((size*8)-pad-1) 
    (fun i ->
      Char.chr (to_int @@ 
        logand 0xFFL @@
        shift_right_logical 
          (field memory v (i/8)) 
          ((i mod 8)*8)))

let trace_val ~bytecode_address ~bytecode_size ~memory v = 
  let open M in
  let open Printf in
  let open Int64 in
  printf "0x%Lx" v;
  let bytecode_address, bytecode_size = of_int bytecode_address, of_int bytecode_size in
  let codeofs v = div (sub v bytecode_address) 4L in
  let in_program v = 
    (rem v 4L = 0L) && v >= bytecode_address && v < (add bytecode_address bytecode_size)
  in
  let header, field = header memory, field memory in
  if is_int v = 1L then printf "=long%Li" (shift_right v 1)
  else if in_program v then printf "=code%Li" (codeofs v)
  else if is_block v = 1L then begin
    let h = header v in
    let tag, size = tag h, to_int (size h) in
    let dump_fields () = 
      if size <> 0 then begin
        printf "=(";
        for i=0 to min (size-1) 3 do
          if i<>0 then printf ", ";
          printf "0x%Lx" (field v i);
        done;
        printf ")"
      end
    in 
    begin
      if tag = closure_tag then 
        printf "=closure[s%i,cod%Li]" size (codeofs (field v 0))
      else if tag = string_tag then
        let str = get_string memory v in
        printf "=string[s%i,L%i]='%s'" size (String.length str) str
      else if tag = double_tag then
        printf "=float[s%i]=%s" size (string_of_float (Int64.float_of_bits (field v 0)))
      else if tag = custom_tag then
        printf "=custom[s%i]" size
      else
        printf "=block<T%Li/s%i>" tag size
    end;
    dump_fields()
  end else printf "=unknown"

let trace ~bytecode_address ~bytecode_size ~stack_address ~memory ~env ~sp ~accu = 
  let trace_val = trace_val ~bytecode_address ~bytecode_size ~memory in
  let sp = Int64.to_int sp in
  let stack_size = (stack_address - sp) / 8 in
  printf "env="; trace_val env; printf "\n";
  printf "accu="; trace_val accu; printf "\n";
  printf " sp=0x%x @%i:\n" sp stack_size;
  for i=0 to min (stack_size-1) 19 do
    printf "[%i] " (stack_size-i); trace_val memory.{ (sp/8)+i }; printf "\n"
  done
    
let make cfg exe = 

  let mem_size_words = 1024*1024 in

  let show_instr = 
    let open Instr in
    let min_ins, max_ins = Bounded.min_bound<opcodes>, Bounded.max_bound<opcodes> in
    let ins = Enum.enum_from_to<opcodes> min_ins max_ins in
    let ins = List.map Show.show<opcodes> ins |> Array.of_list in
    (fun x -> 
      let i = B.to_int x in
      try ins.(i) with _ -> "")
  in

  let wave_cfg = 
    let f = function (n,b) -> if b=1 then n, Waveterm_waves.B
                              else n, Waveterm_waves.H
    in
    Some( 
      [f ("clock",1); f ("clear",1); f ("enable",1); f ("start", 1);
       f ("error",1);
       "state",Waveterm_waves.I Zinc.state_str;
       "pc",Waveterm_waves.U;
       "instruction",Waveterm_waves.F show_instr] @
      Zinc.I.(to_list @@ map f t) @ 
      Zinc.O.(to_list @@ map f t) @
      List.map (fun s -> s,Waveterm_waves.U) ["nfuncs"; "nvars"; "count"; "count_next" ])
  in

  let circ, sim, i, o, n = Z.make "zinc" Zinc.zinc in
  let sim, waves = 
    if cfg.waves then 
      let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in
      sim, Some(waves)
    else sim, None
  in

  let open Zinc.Memory.I in
  let open Zinc.Memory.O in
  let open Zinc.I in
  let open Zinc.O in

  let i = Zinc.I.map If.input i in
  let o = Zinc.O.map If.output o in
  let n = Zinc.O.map If.output n in

  let memory, bytecode_address, atom_table_address, globals_address, 
      c_heap_address, heap_address, stack_address = 
    init_memory exe mem_size_words
  in
  let () = C_runtime.init c_heap_address in

  let trace = 
    let bytecode_size = (Array.length exe.Load.code * 4) in
    (fun () ->
      trace ~bytecode_address ~bytecode_size ~stack_address ~memory
        ~env:o.env#i64 ~sp:o.sp#i64 ~accu:o.sp#i64)
  in

  S.reset sim;
  i.bytecode_start_address#i bytecode_address;
  i.atom_table_address#i (atom_table_address + 8);
  i.globals_start_address#i (globals_address + 8);
  i.heap_start_address#i heap_address;
  i.stack_start_address#i stack_address;
  i.start#i 1;

  let log_mem_access cycle rw addr data sp =
    if cfg.mem_trace then begin
      let offs, typ = 
        if addr < atom_table_address then (addr-0)*2, "BYTE"
        else if addr < globals_address then addr-atom_table_address, "ATOM"
        else if addr < heap_address then addr-globals_address, "GLBL"
        else if addr >= (sp-8) then stack_address - addr - 1, "STCK"
        else addr - heap_address, "HEAP"
      in
      printf "[%-8i] %s %s @[%.8x | %.8x] = %.16Lx [sp=%i]\n"
        cycle (if rw=0 then "R" else "W") typ addr offs data sp
    end
  in

  let run () = 
    let cycle = ref 0 in
    let stop = ref false in
    let instr_no = ref 1 in
    while not !stop && o.error#i <> 1 do
      (* instruction trace *)
      if o.state#i = 2 then begin
        try
          (if cfg.state_trace then printf "\n##%i\n" !instr_no; incr instr_no);
          (if cfg.instr_trace then 
            printf "%6i  %s\n%!"
              ((o.pc#i/4)-1)
              (Show.show<Instr.opcodes> 
                (Enum.to_enum<Instr.opcodes> o.instruction#i)));
          (if cfg.state_trace then trace ());
        with _ -> begin
          stop := true;
          printf "      INVALID\n%!"
        end
      end;
      S.cycle sim;
      (* memory accesses *)
      i.memory_i.memory_ready#i 0;
      if n.memory_o.memory_request#i <> 0 then begin
        let addr = n.memory_o.memory_address#i in
        let rw = n.memory_o.memory_read_write#i in
        let sp = o.sp#i in
        if rw=0 then begin (* read *)
          let data = memory.{addr lsr 3} in
          log_mem_access !cycle rw addr data sp;
          i.memory_i.memory_data_in#i64 data;
        end else begin (* write *)
          let data = n.memory_o.memory_data_out#i64 in
          log_mem_access !cycle rw addr data sp;
          memory.{addr lsr 3} <- data;
        end;
        i.memory_i.memory_ready#i 1;
      end;
      (* c-calls *)
      i.c_call_ready#i 0;
      if n.c_call_request#i = 1 then begin
        let prim = n.c_call_prim#i in
        begin if cfg.instr_trace then 
          Printf.printf "c_call_request: [%i]%s\n" prim exe.Load.prim.(prim);
        end;
        let value = C_runtime.run 
          exe prim
          C_runtime.({
            env=o.env#i64;
            accu=o.accu#i64;
            sp=o.sp#i;
            memory=memory;
          })
        in
        i.c_call_result#i64 value;
        i.c_call_ready#i 1;
      end;
      
      i.start#i 0;
      incr cycle;
    done
  in
  let () = 
    try run () 
    with 
    | Failure x -> printf "\n\nEXN: %s\n\n%!" x
    | _ -> printf "\n\nEXN %s\n\n%!" (Printexc.get_backtrace())
  in
 
  begin
    match waves with
    | None -> ()
    | Some(waves) ->
      Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))
  end


