(* compile hardware design from [Interp] *)

open Base
open Interp

(* Because we are building a 64 bit interpreter. We should look at a 32 bit one
   as well. *)
let dbits = 64
let _initr x = Array.init Machine.num_machine_registers ~f:(fun _ -> x)
let getr a i = a.(Machine.Register.Variants.to_rank i)
let setr a i v = a.(Machine.Register.Variants.to_rank i) <- v
let _initc x = Array.init Machine.num_cache_spaces ~f:(fun _ -> x)
let _getc a i = a.(Machine.Cache.Variants.to_rank i)
let _setc a i v = a.(Machine.Cache.Variants.to_rank i) <- v

module Expression = struct
  let is_const = function
    | Const _ -> true
    | _ -> false
  ;;

  let const_value = function
    | Const n -> n
    | _ -> failwith "expr is not a constant"
  ;;

  let const_equals e n =
    match e with
    | Const m when n = m -> true
    | _ -> false
  ;;

  let rec deps e =
    match e with
    | Op (_, e0, e1) -> deps e0 @ deps e1
    | Val id -> [ id ]
    | Const _ -> []
  ;;

  let eval op e0 e1 =
    if is_const e0 && is_const e1
    then (
      let e0, e1 = const_value e0, const_value e1 in
      match op with
      | "+" -> Some (Const (e0 + e1))
      | "-" -> Some (Const (e0 - e1))
      | "*" -> Some (Const (e0 * e1))
      | "/" -> Some (Const (e0 / e1))
      | "%" -> Some (Const (e0 % e1))
      | "&" -> Some (Const (e0 land e1))
      | "|" -> Some (Const (e0 lor e1))
      | "^" -> Some (Const (e0 lxor e1))
      | "~" -> Some (Const (lnot e0))
      | "<<" -> Some (Const (e0 lsl e1))
      | ">>" -> Some (Const (e0 lsr e1))
      | ">>+" -> Some (Const (e0 asr e1))
      | "==" -> Some (Const (if e0 = e1 then 1 else 0))
      | "<>" -> Some (Const (if e0 <> e1 then 1 else 0))
      | _ -> None)
    else None
  ;;

  let rec simplify e =
    match e with
    | Op (op, e0, e1) ->
      let e0, e1 = simplify e0, simplify e1 in
      (match eval op e0 e1 with
      | Some e -> e
      | None ->
        (match op with
        | "+" when const_equals e0 0 -> e1 (* a + 0 = a *)
        | "+" when const_equals e1 0 -> e0 (* 0 + a = a *)
        | "-" when const_equals e1 0 -> e0 (* a - 0 = a *)
        | "*" when const_equals e1 1 -> e0 (* a * 1 = a *)
        | "*" when const_equals e0 1 -> e1 (* 1 * a = a *)
        | "/" when const_equals e1 1 -> e0 (* a / 1 = a *)
        | "<<" when const_equals e1 0 -> e1 (* a lsl 0 = a *)
        | ">>" when const_equals e1 0 -> e1 (* a lsr 0 = a *)
        | ">>+" when const_equals e1 0 -> e1 (* a asr 0 = a *)
        | _ -> Op (op, e0, e1)))
    | Const _ -> e
    | Val _ -> e
  ;;

  module S = Hardcaml.Signal

  let rec compile env = function
    | Interp.Op (op, a', b') ->
      let a, b =
        try compile env a', compile env b' with
        | _ -> failwith "failed to look up subexpression"
      in
      (match op with
      | "+" -> S.( +: ) a b
      | "-" -> S.( -: ) a b
      | ">>" -> S.srl a (const_value b')
      | ">>+" -> S.sra a (const_value b')
      | "<<" -> S.sll a (const_value b')
      | _ -> failwith ("unknown expression operator '" ^ op ^ "'"))
    | Val id ->
      (match Map.find env id with
      | Some x -> x
      | None -> failwith ("cant find variable " ^ Int.to_string id))
    | Const x -> S.consti ~width:dbits x
  ;;
end

module Statement = struct
  let rec deps x =
    match x with
    | Get_reg (_, _) -> `cmd (x, [])
    | Set_reg (_, value) -> `cmd (x, Expression.deps value)
    | Get_mem (_, _, addr) -> `cmd (x, Expression.deps addr)
    | Set_mem (_, addr, value) -> `cmd (x, Expression.deps addr @ Expression.deps value)
    | Cond (c, t, f) ->
      `cond (x, Expression.deps c, List.map t ~f:deps, List.map f ~f:deps)
    | Iter (_, _, f, t, c) ->
      `iter (x, Expression.deps f @ Expression.deps t, List.map c ~f:deps)
  ;;

  (* XXX I am not sure if this is either needed or useful in it's current form.*)
  let _ = deps

  let rec simplify_stat = function
    | Get_reg (id, reg) -> Get_reg (id, reg)
    | Set_reg (reg, value) -> Set_reg (reg, Expression.simplify value)
    | Get_mem (id, cache, addr) -> Get_mem (id, cache, Expression.simplify addr)
    | Set_mem (cache, addr, value) ->
      Set_mem (cache, Expression.simplify addr, Expression.simplify value)
    | Cond (c, t, f) -> Cond (Expression.simplify c, simplify_stats t, simplify_stats f)
    | Iter (ud, id, f, t, c) ->
      Iter (ud, id, Expression.simplify f, Expression.simplify t, simplify_stats c)

  and simplify_stats x = List.map x ~f:simplify_stat

  let simplify st = { st with cmd = simplify_stats st.cmd }
end

(* 
      Lets try to understand the problems here.

      We are temporary values (ie _0, _1 etc) with calls to
      Get_reg and Get_mem *only*.

      For Get_mem these become register assignments and a state update.
      Relative to Set_mem, therefore, memory accesses and the generated
      values are serialised.

      For Get_reg this reflects the current value of a register.
      We use these values in combinatorial expressions passed
      to Set_reg, Set_mem and also used in the looping and if constructs.

      A Set_reg does not generate a new state, rather we update the 
      combinatorial expression.  When a Set_mem comes a long we do need
      to update the register.  At this point the combinatorial expressions
      become invalid.  Is that OK?  I guess they are committed at that 
      point so maybe it is.

      None-the-less the previous temporary value *IS* still in scope
      and is no longer valid.  Somehow we will need to latch the value
      and start using it instead in potential future expressions.
      Note that it is very likely the old value wont be used so
      we would hope this register could disappear somehow.

      An example sequence.

      _0 = sp;
      sp = _0 + 4; // s = sp + 4, but not yet committed
      _1 = sp;     // read new sp value
      pc = _0;     // pc = initial value of sp as read at start
      accu = _1;   // accu = new value of sp ie sp+4

      No states are generated here.

      This is affected by an intermediate state being generated

      _0 = sp;
      sp = _0 + 4;
      _1 = program[0]; // this will insert a state
      _2 = sp;
      pc = _0;         // pc = sp (new value) ** BAD VALUE **
      accu = _2;       // OK

  *)

(* Lets try to simplify things into passes.

      1) Split into states.  We should be able to generate
         the statemachine structure before we start.

      2) We could optionally derive the liveness of variables
         within states, but this is an optimisation.

      3) Generate logic for each state.  Understanding where
         variables are generated and used (ie if the states are
         different) will help in creating the logic structures)

  *)

open Hardcaml
open Signal

type env =
  { id_to_wire : t Map.M(Int).t
  ; regs : t array
  }

let _init_env () =
  { id_to_wire = Map.empty (module Int)
  ; regs =
      List.map Machine.Register.all ~f:(fun r ->
          let name = Machine.Register.sexp_of_t r |> Sexp.to_string_hum in
          zero dbits -- name)
      |> Array.of_list
  }
;;

type state =
  { n : int
  ; next :
      [ `next of int (* statemachine jumps (generally end of sequence?) *)
      | `mem of sp_cmd * int (* memory operation *)
      | `branch of sp_cmd * int * int (* branch *)
      ]
  ; instrs : sp_cmd list
  }

let print_states sts =
  List.iter sts ~f:(fun st ->
      let open Printf in
      Stdio.printf "****** %i\n" st.n;
      Interp.State_poly.(print { id = 0; cmd = st.instrs });
      Stdio.printf
        "next = %s\n"
        (match st.next with
        | `next i -> sprintf "%i" i
        | `mem (_, i) -> sprintf "%i [mem]" i
        | `branch (_, a, b) -> sprintf "%i/%i [branch]" a b);
      Stdio.printf "\n")
;;

let compile st =
  let cmd = State_poly.normalise st.cmd in
  let rec _compile_stats env = function
    | [] -> env
    (* (psuedo-) update registers and expressions *)
    | Get_reg (id, reg) :: t ->
      let env =
        { env with
          id_to_wire = Map.add_exn env.id_to_wire ~key:id ~data:(getr env.regs reg)
        }
      in
      _compile_stats env t
    | Set_reg (reg, vl) :: t ->
      setr env.regs reg (Expression.compile env.id_to_wire vl);
      _compile_stats env t
    (* memory io and state completion *)
    | Get_mem (id, _, _) :: t ->
      let env =
        { env with id_to_wire = Map.add_exn env.id_to_wire ~key:id ~data:(wire dbits) }
      in
      _compile_stats env t
    | Set_mem (_, _, _) :: t -> _compile_stats env t
    | _ -> failwith "not yet"
  in
  let newst ?(next = `next 0) ?(instrs = []) ?(n = 0) () = { n; next; instrs } in
  let _rs' rstate = function
    | [] -> 0
    | _ -> rstate
  in
  let rec compile_states (cs, rs, sts) instrs = function
    | [] -> cs + 1, rs, newst ~instrs ~n:cs () :: sts
    (* Get/Set_reg; push instructions *)
    | (Get_reg _ as x) :: t -> compile_states (cs, rs, sts) (x :: instrs) t
    | (Set_reg _ as x) :: t -> compile_states (cs, rs, sts) (x :: instrs) t
    (* Get/Set_mem generate new states *)
    | (Set_mem _ as x) :: t ->
      let st =
        newst ~instrs ~next:(`mem (x, if List.is_empty t then rs else cs + 1)) ~n:cs ()
      in
      compile_states (cs + 1, rs, st :: sts) [] t
    | (Get_mem _ as x) :: t ->
      let st =
        newst ~instrs ~next:(`mem (x, if List.is_empty t then rs else cs + 1)) ~n:cs ()
      in
      compile_states (cs + 1, rs, st :: sts) [] t
    (* control flow creates new states *)
    | (Cond (_, a, b) as x) :: t ->
      (* create branch states *)
      let ns, bs0 = cs + 1, cs + 2 in
      let bs1, _, sts = compile_states (bs0, ns, sts) [] a in
      let _, _, sts = compile_states (bs1, ns, sts) [] b in
      (* create cur state *)
      let st = newst ~instrs ~n:cs ~next:(`branch (x, bs0, bs1)) () in
      (* continue with next states *)
      compile_states (ns, rs, st :: sts) [] t
    | Iter _ :: _ -> failwith "not sure about iter yet!"
  in
  (* _compile_stats (init_env ()) cmd*)
  let _, _, sts = compile_states (0, 0, []) [] cmd in
  let sts = List.sort ~compare:(fun a b -> Int.compare a.n b.n) sts in
  let () = print_states sts in
  ()
;;
