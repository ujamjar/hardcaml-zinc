(* Do deeply shocking things with Obj. *)
open Base
module Obj = Caml.Obj

type memory = (Int64.t, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

type repr64 =
  | Flat of Int64.t * Int64.t array
  | Block of Int64.t * repr64 array
  | Int of Int64.t
[@@deriving sexp_of]

module M = Mlvalues.Make (Ops.Int64)

let int64_of_obj o =
  let bitlo = if Obj.is_int o then 1L else 0L in
  let valhi = Int64.of_int (Obj.magic o : int) in
  Int64.(shift_left valhi 1 lor bitlo)
;;

let rec repr64_of_obj ?(closure = true) o =
  if Obj.is_block o
  then (
    let tag, size = Obj.tag o, Obj.size o in
    if (not closure) && tag = Obj.closure_tag
    then Int M.val_unit
    else if tag < Obj.no_scan_tag
    then
      Block
        ( M.make_header (Int64.of_int size) M.white (Int64.of_int tag)
        , Array.(init size ~f:(fun i -> repr64_of_obj ~closure (Obj.field o i))) )
    else
      Flat
        ( M.make_header (Int64.of_int size) M.white (Int64.of_int tag)
        , Array.(init size ~f:(fun i -> int64_of_obj (Obj.field o i))) ))
  else Int Int64.(shift_left (of_int (Obj.magic o : int)) 1 lor 1L)
;;

(* danger!!! *)
let obj_of_int64 i =
  let open Int64 in
  let x = Obj.repr (to_int (shift_right i 1)) in
  if i land 1L = 1L then x else Obj.add_offset x (-1l)
;;

let rec obj_of_repr64 r =
  match r with
  | Int i -> Obj.repr Int64.(to_int (shift_right i 1))
  | Flat (h, d) ->
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int_exn tag) size in
    (* empty? *)
    for i = 0 to size - 1 do
      Obj.set_field b i (obj_of_int64 d.(i))
    done;
    b
  | Block (h, d) ->
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int_exn tag) size in
    (* empty? *)
    for i = 0 to size - 1 do
      Obj.set_field b i (obj_of_repr64 d.(i))
    done;
    b
;;

let data64_of_repr64 data base_word_offset =
  let is_int = function
    | Int v -> Some v
    | _ -> None
  in
  let rec size = function
    | Int _ -> 1
    | Flat (_, a) -> 1 + Array.length a
    | Block (_, a) ->
      1
      + Array.fold a ~init:0 ~f:(fun acc x ->
            acc
            +
            match is_int x with
            | None -> 1 + size x
            | _ -> 1)
  in
  let size = size data in
  let arr = Array.init size ~f:(fun _ -> 0L) in
  let pos = ref 0 in
  let push d =
    let p = !pos in
    arr.(p) <- d;
    Int.incr pos;
    p
  in
  let rec layout = function
    | Int int_val -> push int_val
    | Flat (hdr, data) ->
      let size = Array.length data in
      let base = push hdr in
      for i = 0 to size - 1 do
        ignore @@ push data.(i)
      done;
      base
    | Block (hdr, data) ->
      let size = Array.length data in
      let base = push hdr in
      let resv = Array.init size ~f:(fun _ -> push 0L) in
      (* reserve locations *)
      for i = 0 to size - 1 do
        match is_int data.(i) with
        | Some v -> arr.(resv.(i)) <- v
        | None ->
          let ptr = layout data.(i) in
          (* convert to pointer, offset by base *)
          arr.(resv.(i)) <- Int64.of_int ((base_word_offset + ptr + 1) lsl 3)
      done;
      base
  in
  let (_ : int) = layout data in
  assert (size = !pos);
  arr
;;

let repr64_of_data64 ?(closure = true) d p =
  let rec f p =
    if Int64.equal (M.is_block p) 1L
    then (
      let p' = Int64.to_int_exn p / 8 in
      let hdr = d.{p' - 1} in
      let tag = M.tag hdr in
      let size = M.size hdr in
      if (not closure) && Int64.equal tag M.closure_tag
      then Int M.val_unit
      else if Int64.compare tag M.no_scan_tag < 0
      then Block (hdr, Array.init (Int64.to_int_exn size) ~f:(fun i -> f d.{p' + i}))
      else Flat (hdr, Array.init (Int64.to_int_exn size) ~f:(fun i -> d.{p' + i})))
    else Int p
  in
  f p
;;
