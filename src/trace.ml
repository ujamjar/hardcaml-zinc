open Printf
open Machine
module M = Mlvalues.Make (Ops.Int64)

let header memory v = memory.{(Int64.to_int v / 8) - 1}

let field memory v i = memory.{(Int64.to_int v / 8) + i}

let get_string memory v =
  let open Int64 in
  let size = to_int @@ M.size (header memory v) in
  let pad = to_int @@ shift_right_logical (field memory v (size - 1)) 56 in
  String.init
    ((size * 8) - pad - 1)
    (fun i ->
      Char.chr
        ( to_int @@ logand 0xFFL
        @@ shift_right_logical (field memory v (i / 8)) (i mod 8 * 8) ))

let showfields = ref false

let root ?(chan = stdout) st v =
  let open Int64 in
  let open Ops.Int64 in
  let in_program v =
    let v = to_int v in
    v >= st.mapping.code_address
    && v <= st.mapping.code_address + st.mapping.code_size
  in
  let rec f pad v =
    let get i = st.memory.{to_int i / 8} in
    if M.is_int v = 1L then fprintf chan "%si%Li\n" pad (M.int_val v)
    else if in_program v then
      fprintf chan "%sc%Li\n" pad (srl (v -: of_int st.mapping.code_address) 2L)
    else
      let npad = " " ^ pad in
      let hdr = get (v -: 8L) in
      let tag = M.tag hdr in
      let size = M.size hdr in
      fprintf chan "%s[t%Li s%Li] p%.16Lx\n" pad tag size v;
      if tag = M.string_tag then
        fprintf chan "%ss'%s'\n" pad (get_string st.memory v)
      else if tag < M.no_scan_tag then
        for i = 0 to to_int size - 1 do
          f npad (get (v +: of_int (i * 8)))
        done
      else
        for i = 0 to to_int size - 1 do
          fprintf chan "%sd%.16Lx\n" npad (get (v +: of_int (i * 8)))
        done
  in
  f "" v

let value ?(chan = stdout) st v =
  let open M in
  let open Printf in
  let open Int64 in
  let sp = Int64.to_int st.sp in
  if !showfields then fprintf chan "0x%Lx" v
  else fprintf chan "%c" (if is_int v = 1L then 'i' else 'p');
  let bytecode_address, bytecode_size =
    (of_int st.mapping.code_address, of_int st.mapping.code_size)
  in
  let codeofs v = div (sub v bytecode_address) 4L in
  let in_program v =
    rem v 4L = 0L
    && v >= bytecode_address
    && v < add bytecode_address bytecode_size
  in
  let in_stack sp v =
    let v = to_int v in
    v >= sp && v <= st.mapping.stack_address
  in
  let header, field = (header st.memory, field st.memory) in
  let printstr str =
    String.init
      (min 31 (String.length str))
      (fun i ->
        let c = str.[i] in
        if c >= ' ' && c <= '~' then c else '?')
  in
  if is_int v = 1L then fprintf chan "=long%Li" (shift_right v 1)
  else if in_program v then fprintf chan "=code@%Li" (codeofs v)
  else if in_stack sp v then
    fprintf chan "=stack_%i" ((st.mapping.stack_address - to_int v) / 8)
  else if is_block v = 1L then
    let h = header v in
    let tag, size = (tag h, to_int (size h)) in
    let dump_fields () =
      if size <> 0 && !showfields then (
        fprintf chan "=(";
        for i = 0 to min (size - 1) 20 do
          if i <> 0 then fprintf chan ", ";
          fprintf chan "0x%Lx" (field v i)
        done;
        fprintf chan ")" )
    in
    if tag = closure_tag then (
      fprintf chan "=closure[s%i,cod%Li]" size (codeofs (field v 0));
      dump_fields () )
    else if tag = string_tag then (
      let str = get_string st.memory v in
      fprintf chan "=string[s%iL%i]'%s'" size (String.length str) (printstr str);
      dump_fields () )
    else if tag = double_tag then (
      fprintf chan "=float[s%i]=%g" size (Int64.float_of_bits (field v 0));
      dump_fields () )
    else if tag = double_array_tag then (
      fprintf chan "=floatarray[s%i]" size;
      dump_fields () )
    else if tag = custom_tag then (
      fprintf chan "=custom[s%i]" size;
      dump_fields () )
    else if tag = abstract_tag then (
      fprintf chan "=abstract[s%i]" size;
      dump_fields () )
    else fprintf chan "=block<T%Li/s%i>" tag size
  else fprintf chan "=unknown"

let machine ?(chan = stdout) st =
  let sp = Int64.to_int st.sp in
  let trapsp = Int64.to_int st.trapsp in
  let eargs = Int64.to_int st.extra_args in
  let stack_size = (st.mapping.stack_address - sp) / 8 in
  let trap_stack_size = (st.mapping.stack_address - trapsp) / 8 in
  fprintf chan "env=";
  value st st.env;
  fprintf chan "\n";
  fprintf chan "accu=";
  value st st.accu;
  fprintf chan "\n";
  if !showfields then
    fprintf chan " sp=0x%x @%i: trapsp=0x%x @%i extra_args=%i\n" sp stack_size
      trapsp trap_stack_size eargs
  else
    fprintf chan " sp=%i: trapsp=%i extra_args=%i\n" stack_size trap_stack_size
      eargs;
  for i = 0 to min (stack_size - 1) 15 do
    fprintf chan "[%i] " (stack_size - i);
    value st st.memory.{(sp / 8) + i};
    fprintf chan "\n"
  done

let get_instr memory pc =
  let instr = memory.{pc / 2} in
  Ops.Int64.(sra (if pc mod 2 = 0 then sll instr 32L else instr) 32L)

let instr ?(chan = stdout) st =
  let pc = Int64.to_int st.pc / 4 in
  let instr = get_instr st.memory pc in
  let instr = Opcode.of_int (Int64.to_int instr) in
  let get_arg n = get_instr st.memory (pc + 1 + n) in
  fprintf chan "%6i  %s" pc (Opcode.to_string instr);
  match instr with
  | PUSHACC | ACC | POP | ASSIGN | PUSHENVACC | ENVACC | PUSH_RETADDR | APPLY
  | APPTERM1 | APPTERM2 | APPTERM3 | RETURN | GRAB | PUSHGETGLOBAL | GETGLOBAL
  | SETGLOBAL | PUSHATOM | ATOM | MAKEBLOCK1 | MAKEBLOCK2 | MAKEBLOCK3
  | MAKEFLOATBLOCK | GETFIELD | SETFIELD | GETFLOATFIELD | SETFLOATFIELD
  | BRANCH | BRANCHIF | BRANCHIFNOT | PUSHTRAP | CONSTINT | PUSHCONSTINT
  | OFFSETINT | OFFSETREF | OFFSETCLOSURE | PUSHOFFSETCLOSURE ->
      fprintf chan " %Ld\n" (get_arg 0)
  | APPTERM | CLOSURE | CLOSUREREC | PUSHGETGLOBALFIELD | GETGLOBALFIELD
  | MAKEBLOCK | BEQ | BNEQ | BLTINT | BLEINT | BGTINT | BGEINT | BULTINT
  | BUGEINT ->
      fprintf chan " %Ld, %Ld\n" (get_arg 0) (get_arg 1)
  | C_CALLN | C_CALL1 | C_CALL2 | C_CALL3 | C_CALL4 | C_CALL5 ->
      if instr = C_CALLN then (
        fprintf chan "%Ld, " (get_arg 0);
        fprintf chan " %s\n" st.exe.Load.prim.(Int64.to_int (get_arg 1)) )
      else fprintf chan " %s\n" st.exe.Load.prim.(Int64.to_int (get_arg 0))
  | _ -> fprintf chan "\n"
