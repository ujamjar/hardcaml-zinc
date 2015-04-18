(* XXX copied from bytecomp/symtable.ml - MUST MATCH EXACTLY *)
type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t }  (* The table of already numbered objects *)

type bytecode_exe = 
  {
    toc : (bytes * int) list;
    crcs : (string * Digest.t option) list;
    dplt : string;
    dlls : string;
    code : Int32.t array;
    prim : string array;
    data : string;
    symb : Ident.t numtable option (* XXX err - numtable is not exposed in compiler-libs *)
  }

let empty = 
  {
    toc = [];
    crcs = [];
    dplt = "";
    dlls = "";
    code = [||];
    prim = [||];
    data = "";
    symb = None;
  }

let prims str = 
  let pos = ref 0 in
  let prims = ref [] in
  while !pos < String.length str do
    let i = String.index_from str !pos '\000' in
    prims := (String.sub str !pos (i - !pos)) :: !prims;
    pos := i + 1
  done;
  Array.of_list @@ List.rev !prims

let byte_codes str = 
  let len = String.length str in
  Array.init (len/4) 
    (fun i ->
      let a = Array.init 4 
        (fun j -> Int32.shift_left (Int32.of_int (Char.code str.[(i*4)+j])) (j*8))
      in
      Array.fold_left Int32.logor 0l a)

let make_header size colour tag = 
  let open Int64 in
  logor
    (shift_left (of_int size) 10)
    (logor
      (shift_left (of_int colour) 8)
      (of_int tag))

type repr64 = 
  [ `f of int64 * int64 array 
  | `b of int64 * repr64 array 
  | `i of int64 ]

let rec get_repr64 o =
  if Obj.is_block o then
    let tag,size = Obj.tag o, Obj.size o in
    if tag < Obj.no_scan_tag then
      `b(make_header size Instr.white tag, Array.(init size (fun i -> get_repr64 (Obj.field o i))))
    else
      `f(make_header size Instr.white tag, Array.(init size (fun i ->
        let o = Obj.field o i in
        let bitlo = if Obj.is_int o then 1L else 0L in
        let valhi = Int64.of_int (Obj.magic o : int) in
        Int64.(logor (shift_left valhi 1) bitlo))))
  else
    `i Int64.(logor (shift_left (of_int (Obj.magic o : int)) 1) 1L)

let get_data64 data base_word_offset = 
  let is_int = function `i v -> Some(v) | _ -> None in
  let rec size = function
    | `i _ -> 1
    | `f(_, a) -> 1 + Array.length a
    | `b(_, a) -> 
      1 + (Array.fold_left 
            (fun acc x -> 
              acc + (match is_int x with None -> (1 + size x) | _ -> 1)) 0 a)
  in
  let size = size data in
  let arr = Array.init size (fun _ -> 0L) in
  let pos = ref 0 in
  let push = 
    (fun d -> 
      let p = !pos in
      arr.(p) <- d;
      incr pos;
      p)
  in
  let rec layout = function
    | `i(int_val) -> push int_val
    | `f(hdr,data) -> begin
      let size = Array.length data in
      let base = push hdr in
      for i=0 to size - 1 do
        ignore @@ push data.(i)
      done;
      base
    end
    | `b(hdr,data) -> begin
      let size = Array.length data in
      let base = push hdr in
      let resv = Array.init size (fun _ -> push 0L) in (* reserve locations *)
      for i=0 to size-1 do
        match is_int data.(i) with
        | Some(v) -> arr.( resv.(i) ) <- v
        | None -> 
          let ptr = layout data.(i) in
          (* convert to pointer, offset by base *)
          arr.( resv.(i) ) <- Int64.of_int ((base_word_offset + ptr + 1) lsl 3) 
      done;
      base
    end
  in
  let _ = layout data in
  assert (size = !pos);
  arr

let get_global_data64 exe offset = 
  get_data64 (get_repr64 (Obj.repr (Marshal.from_string exe.data 0))) offset

let bytecode_exe exe_name = 
  Symtable.reset ();
  Bytesections.reset ();
  let f = open_in exe_name in
  Bytesections.read_toc f;
  let toc = Bytesections.toc () in
  let str s = try Bytesections.read_section_string f s with Not_found -> "" in
  let exe = 
    {
      toc = toc;
      crcs = 
        (Obj.magic (Bytesections.read_section_struct f "CRCS") : (string * Digest.t option) list);
      dplt = str "DPLT";
      dlls = str "DLLS";
      code = byte_codes (str "CODE");
      prim = prims (str "PRIM");
      data = str "DATA";
      symb = 
        Some (Obj.magic (Bytesections.read_section_struct f "SYMB") : Ident.t numtable);
    }
  in
  close_in f;
  exe


