(* Do deeply shocking things with Obj. *)
open Base
module Obj = Caml.Obj

type t =
  | Flat of
      { header : Int64.t
      ; data : Int64.Hex.t array
      }
  | Block of
      { header : Int64.t
      ; data : t array
      }
  | Int of Int64.t
[@@deriving sexp_of]

module M = Mlvalues.Make (Ops.Int64)

(* !!!danger!!! *)
let obj_of_int64 i =
  let x = Obj.repr Int64.(to_int_exn (shift_right i 1)) in
  if Int64.(i land 1L = 1L) then x else Obj.add_offset x (-1l)
;;

let int64_of_obj o =
  let bitlo = if Obj.is_int o then 1L else 0L in
  let valhi = Int64.of_int (Obj.magic o : int) in
  Int64.(shift_left valhi 1 lor bitlo)
;;

let rec of_obj ?(closure = true) o =
  if Obj.is_block o
  then (
    let tag, size = Obj.tag o, Obj.size o in
    if (not closure) && tag = Obj.closure_tag
    then Int M.val_unit
    else if tag < Obj.no_scan_tag
    then
      Block
        { header = M.make_header (Int64.of_int size) M.white (Int64.of_int tag)
        ; data = Array.(init size ~f:(fun i -> of_obj ~closure (Obj.field o i)))
        }
    else
      Flat
        { header = M.make_header (Int64.of_int size) M.white (Int64.of_int tag)
        ; data = Array.(init size ~f:(fun i -> int64_of_obj (Obj.field o i)))
        })
  else Int Int64.(shift_left (of_int (Obj.magic o : int)) 1 lor 1L)
;;

let rec to_obj r =
  match r with
  | Int i -> Obj.repr Int64.(to_int_exn (shift_right i 1))
  | Flat { header = h; data = d } ->
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int_exn tag) size in
    (* empty? *)
    for i = 0 to size - 1 do
      Obj.set_field b i (obj_of_int64 d.(i))
    done;
    b
  | Block { header = h; data = d } ->
    let size = Array.length d in
    let tag = M.tag h in
    let b = Obj.new_block (Int64.to_int_exn tag) size in
    (* empty? *)
    for i = 0 to size - 1 do
      Obj.set_field b i (to_obj d.(i))
    done;
    b
;;

let to_data64 data base_word_offset =
  let is_int = function
    | Int v -> Some v
    | _ -> None
  in
  let rec size = function
    | Int _ -> 1
    | Flat { data; _ } -> 1 + Array.length data
    | Block { data; _ } ->
      1
      + Array.fold data ~init:0 ~f:(fun acc x ->
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
    | Flat { header; data } ->
      let size = Array.length data in
      let base = push header in
      for i = 0 to size - 1 do
        ignore @@ push data.(i)
      done;
      base
    | Block { header; data } ->
      let size = Array.length data in
      let base = push header in
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

let of_data64 ?(closure = true) d p =
  let rec f p =
    if Int64.equal (M.is_block p) 1L
    then (
      let p' = Int64.to_int_exn p / 8 in
      let header = d.{p' - 1} in
      let tag = M.tag header in
      let size = M.size header in
      if (not closure) && Int64.equal tag M.closure_tag
      then Int M.val_unit
      else if Int64.compare tag M.no_scan_tag < 0
      then
        Block
          { header; data = Array.init (Int64.to_int_exn size) ~f:(fun i -> f d.{p' + i}) }
      else
        Flat
          { header; data = Array.init (Int64.to_int_exn size) ~f:(fun i -> d.{p' + i}) })
    else Int p
  in
  f p
;;
