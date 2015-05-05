open Printf
open Machine

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

let showfields = ref false

let root file memory v = 
  let open Int64 in
  let open Ops.Int64 in
  let rec f pad v = 
    let get i = memory.{ to_int i / 8 } in
    if M.is_int v = 1L then fprintf file "%si%Li\n" pad (M.int_val v)
    else begin
      let npad = " " ^ pad in
      let hdr = get (v -: 8L) in
      let tag = M.tag hdr in
      let size = M.size hdr in
      fprintf file "%s[t%Li s%Li] p%.16Lx\n" pad tag size v;
      if tag = M.string_tag then
        fprintf file "%ss'%s'\n" pad (get_string memory v)
      else if tag < M.no_scan_tag then
        for i=0 to to_int size - 1 do
          f npad (get (v +: (of_int (i*8))))
        done
      else
        for i=0 to to_int size - 1 do
          fprintf file "%sd%.16Lx\n" npad (get (v +: (of_int (i*8))))
        done
    end
  in
  f "" v

let value m sp v = 
  let open M in
  let open Printf in
  let open Int64 in
  (if !showfields then
    printf "0x%Lx" v
  else
    printf "%c" (if is_int v=1L then 'i' else 'p'));
  let bytecode_address, bytecode_size = of_int m.code_address, of_int m.code_size in
  let codeofs v = div (sub v bytecode_address) 4L in
  let in_program v = 
    (rem v 4L = 0L) && v >= bytecode_address && v < (add bytecode_address bytecode_size)
  in
  let in_stack sp v = 
    let v = to_int v in
    v >= sp && v <= m.stack_address
  in
  let header, field = header m.memory, field m.memory in
  let printstr str = 
    String.init (min 31 (String.length str)) (fun i ->
      let c = String.get str i in
      if c >= ' ' && c <= '~' then c else '?')
  in
  if is_int v = 1L then printf "=long%Li" (shift_right v 1)
  else if in_program v then printf "=code@%Li" (codeofs v)
  else if in_stack sp v then printf "=stack_%i" ((m.stack_address - to_int v) / 8)
  else if is_block v = 1L then begin
    let h = header v in
    let tag, size = tag h, to_int (size h) in
    let dump_fields () = 
      if size <> 0 && !showfields then begin
        printf "=(";
        for i=0 to min (size-1) 20 do
          if i<>0 then printf ", ";
          printf "0x%Lx" (field v i);
        done;
        printf ")"
      end
    in 
    if tag = closure_tag then begin
      printf "=closure[s%i,cod%Li]" size (codeofs (field v 0));
      dump_fields()
    end else if tag = string_tag then begin
      let str = get_string m.memory v in
      printf "=string[s%iL%i]'%s'" size (String.length str) (printstr str);
      dump_fields()
    end else if tag = double_tag then begin
      printf "=float[s%i]=%g" size (Int64.float_of_bits (field v 0));
      dump_fields()
    end else if tag = double_array_tag then begin
      printf "=floatarray[s%i]" size;
      dump_fields()
    end else if tag = custom_tag then begin
      printf "=custom[s%i]" size;
      dump_fields()
    end else if tag = abstract_tag then begin
      printf "=abstract[s%i]" size;
      dump_fields()
    end else
      printf "=block<T%Li/s%i>" tag size
  end else printf "=unknown"

let machine ~m ~env ~sp ~accu ~trapsp ~eargs = 
  let trace_val = value m in
  let sp = Int64.to_int sp in
  let trapsp = Int64.to_int trapsp in
  let eargs = Int64.to_int eargs in
  let stack_size = (m.stack_address - sp) / 8 in
  let trap_stack_size = (m.stack_address - trapsp) / 8 in
  printf "env="; trace_val sp env; printf "\n";
  printf "accu="; trace_val sp accu; printf "\n";
  (if !showfields then 
    printf " sp=0x%x @%i: trapsp=0x%x @%i extra_args=%i\n" 
      sp stack_size trapsp trap_stack_size eargs
  else
    printf " sp=%i: trapsp=%i extra_args=%i\n" stack_size trap_stack_size eargs);
  for i=0 to min (stack_size-1) 15 do
    printf "[%i] " (stack_size-i); trace_val sp m.memory.{ (sp/8)+i }; printf "\n"
  done
    

