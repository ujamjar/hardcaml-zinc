(*
 * Do deeply shocking things with Obj.
 *)

type memory = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 

type repr64 = 
  [ `f of int64 * int64 array 
  | `b of int64 * repr64 array 
  | `i of int64 ]

module M = Mlvalues.Make(Ops.Int64)

let int64_of_obj o = 
  let bitlo = if Obj.is_int o then 1L else 0L in
  let valhi = Int64.of_int (Obj.magic o : int) in
  Int64.(logor (shift_left valhi 1) bitlo)

let rec repr64_of_obj o =
  if Obj.is_block o then
    let tag,size = Obj.tag o, Obj.size o in
    if tag < Obj.no_scan_tag then
      `b(M.make_header (Int64.of_int size) M.white (Int64.of_int tag), 
          Array.(init size (fun i -> repr64_of_obj (Obj.field o i))))
    else
      `f(M.make_header (Int64.of_int size) M.white (Int64.of_int tag), 
        Array.(init size (fun i -> int64_of_obj (Obj.field o i))))
  else
    `i Int64.(logor (shift_left (of_int (Obj.magic o : int)) 1) 1L)

(* danger!!! *)
let obj_of_int64 i = 
  let open Int64 in
  let x = Obj.repr (to_int (shift_right i 1)) in
  if logand i 1L = 1L then x
  else Obj.add_offset x (-1l)

let rec obj_of_repr64 r = 
  match r with
  | `i i -> Obj.repr (Int64.(to_int (shift_right i 1)))
  | `f (h, d) -> 
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int tag) size in (* empty? *)
    for i=0 to size-1 do
      Obj.set_field b i (obj_of_int64 d.(i))
    done;
    b
  | `b(h, d) -> 
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int tag) size in (* empty? *)
    for i=0 to size-1 do
      Obj.set_field b i (obj_of_repr64 d.(i))
    done;
    b

let data64_of_repr64 data base_word_offset = 
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

let repr64_of_data64 d p =
  let rec f p = 
    if M.is_block p = 1L then
      let p' = Int64.to_int p / 8 in
      let hdr = d.{p'-1} in
      let tag = M.tag hdr in
      let size = M.size hdr in
      if tag < M.no_scan_tag then
        `b(hdr, Array.init (Int64.to_int size) (fun i -> f d.{p' + i}))
      else
        `f(hdr, Array.init (Int64.to_int size) (fun i -> d.{p' + i}))
    else
      `i p
  in
  f p

