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
  let heap_address = globals_address + (Array.length globals) in
  let memory = 
    Array.init memory_size_words (fun i ->
      if i<code_size then exe i
      else if i<globals_address then atom (i-code_size)
      else if i<heap_address then globals.(i-globals_address)
      else 0L)
  in
  let stack_address = memory_size_words in
  memory, code_address, atoms_address, globals_address, heap_address, stack_address

let make exe = 

  let mem_size_words = 1024*1024 in

  let show_instr = 
    let open Instr in
    let min_ins, max_ins = Bounded.min_bound<opcodes>, Bounded.max_bound<opcodes> in
    let ins = Enum.enum_from_to<opcodes> min_ins max_ins in
    let ins = List.map Show.show<opcodes> ins |> Array.of_list in
    (fun x -> try ins.(B.to_int x) with _ -> "")
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
      Zinc.O.(to_list @@ map f t) )
  in

  let circ, sim, i, o, n = Z.make "zinc" Zinc.zinc in
  let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in

  let open Zinc.Memory.I in
  let open Zinc.Memory.O in
  let open Zinc.I in
  let open Zinc.O in

  let i = Zinc.I.map If.input i in
  let o = Zinc.O.map If.output o in
  let n = Zinc.O.map If.output n in

  let memory, bytecode_address, atom_table_address, globals_address, heap_address, stack_address = 
    init_memory exe mem_size_words
  in

  S.reset sim;
  i.bytecode_start_address#i bytecode_address;
  i.atom_table_address#i atom_table_address;
  i.globals_start_address#i globals_address;
  i.heap_start_address#i heap_address;
  i.stack_start_address#i stack_address;
  i.start#i 1;

  let log_mem_access cycle rw addr data sp =
    let offs, typ = 
      let addr = addr lsr 3 in
      if addr < atom_table_address then (addr-0)*2, "BYTE"
      else if addr < globals_address then addr-atom_table_address, "ATOM"
      else if addr < heap_address then addr-globals_address, "GLBL"
      else if addr >= (sp-1) then stack_address - addr - 1, "STCK"
      else addr, "HEAP"
    in
    printf "[%-8i] %s %s @[%.8x | %.8x] = %.16Lx [sp=%i]\n"
      cycle (if rw=0 then "R" else "W") typ addr offs data sp
  in

  let cycle = ref 0 in
  while o.error#i <> 1 do
    if o.state#i = 2 then begin
      try
        printf "      %s\n" 
          (Show.show<Instr.opcodes> 
            (Enum.to_enum<Instr.opcodes> o.instruction#i));
      with _ -> 
        printf "      INVALID\n"
    end;
    S.cycle sim;
    i.memory_i.memory_ready#i 0;
    if n.memory_o.memory_request#i <> 0 then begin
      let addr = n.memory_o.memory_address#i in
      let rw = n.memory_o.memory_read_write#i in
      let sp = o.sp#i in
      if rw=0 then begin (* read *)
        let data = memory.(addr lsr 3) in
        log_mem_access !cycle rw addr data sp;
        i.memory_i.memory_data_in#i64 data;
      end else begin (* write *)
        let data = n.memory_o.memory_data_out#i64 in
        log_mem_access !cycle rw addr data sp;
        memory.(addr lsr 3) <- data;
      end;
      i.memory_i.memory_ready#i 1;
    end;
    
    i.start#i 0;
    incr cycle;
  done;
  
  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

