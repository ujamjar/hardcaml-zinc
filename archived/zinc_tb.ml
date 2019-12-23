open Printf
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform
module Z = Cyclesim.With_interface (Zinc.I) (Zinc.O)

type cfg = {
  waves : bool;
  instr_trace : bool;
  state_trace : bool;
  mem_trace : bool;
}

let make cfg exe =
  let mem_size_words = 1024 * 1024 in

  let _show_instr =
    let ins =
      Base.List.map Opcode.all ~f:Opcode.to_string |> Base.Array.of_list
    in

    fun x ->
      let i = Bits.to_int x in
      try ins.(i) with _ -> ""
  in

  (* let wave_cfg =
   *   let f = function
   *     | n, b -> if b = 1 then (n, Waveterm_waves.B) else (n, Waveterm_waves.H)
   *   in
   *   Some
   *     ( [
   *         f ("clock", 1);
   *         f ("clear", 1);
   *         f ("enable", 1);
   *         f ("start", 1);
   *         f ("error", 1);
   *         ("state", Waveterm_waves.I Zinc.state_str);
   *         ("pc", Waveterm_waves.U);
   *         ("instruction", Waveterm_waves.F show_instr);
   *       ]
   *     @ Zinc.I.(to_list @@ map t ~f)
   *     @ Zinc.O.(to_list @@ map t ~f)
   *     @ List.map
   *         (fun s -> (s, Waveterm_waves.U))
   *         [ "nfuncs"; "nvars"; "count"; "count_next" ] )
   * in *)
  let sim = Z.create Zinc.zinc in
  let waves, sim =
    if cfg.waves then
      let waves, sim = Waveform.create sim in
      (Some waves, sim)
    else (None, sim)
  in

  let open Zinc.Memory.I in
  let open Zinc.Memory.O in
  let open Zinc.I in
  let open Zinc.O in
  let i =
    Zinc.I.map (Cyclesim.inputs sim) ~f:(fun i d ->
        i := Bits.consti ~width:(Bits.width !i) d)
  in
  let i64 =
    Zinc.I.map (Cyclesim.inputs sim) ~f:(fun i d ->
        i := Bits.consti64 ~width:(Bits.width !i) d)
  in
  let o = Zinc.O.map (Cyclesim.outputs sim) ~f:(fun o () -> Bits.to_int !o) in
  let n =
    Zinc.O.map (Cyclesim.outputs ~clock_edge:After sim) ~f:(fun o () ->
        Bits.to_int !o)
  in
  let n64 =
    Zinc.O.map (Cyclesim.outputs ~clock_edge:After sim) ~f:(fun o () ->
        Bits.to_int64 !o)
  in
  let o64 =
    Zinc.O.map (Cyclesim.outputs sim) ~f:(fun o () ->
        Bits.to_int !o |> Int64.of_int)
  in

  let mapping, memory = init_memory exe mem_size_words in

  let trace () =
    Trace.machine
      {
        Machine.empty with
        memory;
        mapping;
        env = o64.env ();
        sp = o64.sp ();
        accu = o64.sp ();
      }
  in

  Cyclesim.reset sim;
  i.bytecode_start_address mapping.code_address;
  i.atom_table_address (mapping.atoms_address + 8);
  i.globals_start_address (mapping.globals_address + 8);
  i.heap_start_address mapping.heap_address;
  i.stack_start_address mapping.stack_address;
  i.start 1;

  let log_mem_access cycle rw addr data sp =
    if cfg.mem_trace then
      let offs, typ =
        if addr < mapping.atoms_address then ((addr - 0) * 2, "BYTE")
        else if addr < mapping.globals_address then
          (addr - mapping.atoms_address, "ATOM")
        else if addr < mapping.heap_address then
          (addr - mapping.globals_address, "GLBL")
        else if addr >= sp - 8 then (mapping.stack_address - addr - 1, "STCK")
        else (addr - mapping.heap_address, "HEAP")
      in
      printf "[%-8i] %s %s @[%.8x | %.8x] = %.16Lx [sp=%i]\n" cycle
        (if rw = 0 then "R" else "W")
        typ addr offs data sp
  in

  let run () =
    let cycle = ref 0 in
    let stop = ref false in
    let instr_no = ref 1 in
    while (not !stop) && o.error () <> 1 do
      (* instruction trace *)
      if o.state () = 2 then (
        try
          if cfg.state_trace then printf "\n##%i\n" !instr_no;
          incr instr_no;
          if cfg.instr_trace then
            printf "%6i  %s\n%!"
              ((o.pc () / 4) - 1)
              (o.instruction () |> Opcode.of_int |> Opcode.to_string);
          if cfg.state_trace then trace ()
        with _ ->
          stop := true;
          printf "      INVALID\n%!" );
      Cyclesim.cycle sim;
      (* memory accesses *)
      i.memory_i.memory_ready 0;
      if n.memory_o.memory_request () <> 0 then (
        let addr = n.memory_o.memory_address () in
        let rw = n.memory_o.memory_read_write () in
        let sp = o.sp () in
        ( if rw = 0 then (
          (* read *)
          let data = memory.{addr lsr 3} in
          log_mem_access !cycle rw addr data sp;
          i64.memory_i.memory_data_in data )
        else
          (* write *)
          let data = n64.memory_o.memory_data_out () in
          log_mem_access !cycle rw addr data sp;
          memory.{addr lsr 3} <- data );
        i.memory_i.memory_ready 1 );
      (* c-calls *)
      i.c_call_ready 0;
      if n.c_call_request () = 1 then (
        let prim = n.c_call_prim () in
        if cfg.instr_trace then
          Printf.printf "c_call_request: [%i]%s\n" prim exe.Load.prim.(prim);
        let value =
          match
            C_runtime.run exe prim
              {
                Machine.empty with
                Machine.env = o64.env ();
                accu = o64.accu ();
                sp = o64.sp ();
                memory;
              }
          with
          | `ok v -> v
          | `exn _ -> failwith "c-call exn not implemented"
        in
        i64.c_call_result value;
        i.c_call_ready 1 );

      i.start 0;
      incr cycle
    done
  in
  let () =
    try run () with
    | Failure x -> printf "\n\nEXN: %s\n\n%!" x
    | _ -> printf "\n\nEXN %s\n\n%!" (Printexc.get_backtrace ())
  in

  match waves with
  | None -> ()
  | Some waves -> Hardcaml_waveterm_interactive.run waves
