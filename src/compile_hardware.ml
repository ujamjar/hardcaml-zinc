(* compile hardware design from [Interp] *)

open Base
open Interp

(* Because we are building a 64 bit interpreter. We should look at a 32 bit one
   as well. *)
let dbits = 64

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

  let rec compile lookup = function
    | Interp.Op (op, a', b') ->
      let a, b =
        try compile lookup a', compile lookup b' with
        | _ -> failwith "failed to look up subexpression"
      in
      (match op with
      | "+" -> S.( +: ) a b
      | "-" -> S.( -: ) a b
      | ">>" -> S.srl a (const_value b')
      | ">>+" -> S.sra a (const_value b')
      | "<<" -> S.sll a (const_value b')
      | _ -> failwith ("unknown expression operator '" ^ op ^ "'"))
    | Val id -> lookup id
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

  module Usage = struct
    type t =
      { read_registers : Machine.Register.t list
      ; write_registers : Machine.Register.t list
      ; read_memories : Machine.Cache.t list
      ; write_memories : Machine.Cache.t list
      }

    let z =
      { read_registers = []
      ; write_registers = []
      ; read_memories = []
      ; write_memories = []
      }
    ;;

    let merge a b =
      { read_registers = a.read_registers @ b.read_registers
      ; write_registers = a.write_registers @ b.write_registers
      ; read_memories = a.read_memories @ b.read_memories
      ; write_memories = a.write_memories @ b.write_memories
      }
    ;;

    let rec usage1 st =
      match st with
      | Get_reg (_, reg) -> { z with read_registers = [ reg ] }
      | Set_reg (reg, _) -> { z with write_registers = [ reg ] }
      | Get_mem (_, mem, _) -> { z with read_memories = [ mem ] }
      | Set_mem (mem, _, _) -> { z with write_memories = [ mem ] }
      | Cond (_, t, f) -> merge (usage t) (usage f)
      | Iter (_, _, _, _, body) -> usage body

    and usage st_list = List.fold st_list ~init:z ~f:(fun acc st -> merge acc (usage1 st))

    let create st =
      let a = usage st in
      { read_registers =
          Set.of_list (module Machine.Register) a.read_registers |> Set.to_list
      ; write_registers =
          Set.of_list (module Machine.Register) a.write_registers |> Set.to_list
      ; read_memories = Set.of_list (module Machine.Cache) a.read_memories |> Set.to_list
      ; write_memories =
          Set.of_list (module Machine.Cache) a.write_memories |> Set.to_list
      }
    ;;
  end
end

(* Before we try to optimise anything, lets due a purely sequential statemachine.

   I need to work out how to manage the statemachine representation.
 *)
module Sequential = struct
  open Hardcaml
  open Signal

  module Zinc_register = struct
    (* A bit of internal plumbing so the registers can be exposed as an interface *)
    type 'a t = 'a array [@@deriving sexp_of]

    module Pre : Hardcaml.Interface.Pre with type 'a t = 'a t = struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let map = Array.map
      let map2 a b ~f = Array.init (Array.length a) ~f:(fun i -> f a.(i) b.(i))
      let iter = Array.iter
      let iter2 a b ~f = ignore (map2 a b ~f : unit array)
      let to_list = Array.to_list

      let t =
        Machine.Register.all
        |> List.map ~f:(fun x -> Sexp.to_string_hum (Machine.Register.sexp_of_t x), dbits)
        |> Array.of_list
      ;;
    end

    include Interface.Make (Pre)

    let create reg_spec =
      List.map Machine.Register.all ~f:(fun _ ->
          Always.Variable.reg reg_spec ~enable:vdd ~width:64)
      |> Array.of_list
    ;;

    let var t register = t.(Machine.Register.Variants.to_rank register)
    let get t register = (var t register).Always.Variable.value
  end

  module Command_register = struct
    type t =
      { reg_spec : Reg_spec.t
      ; table : (int, Always.Variable.t) Hashtbl.t
      }

    let create reg_spec = { reg_spec; table = Hashtbl.create (module Int) }

    let var t index =
      match Hashtbl.find t.table index with
      | None ->
        let x = Always.Variable.reg t.reg_spec ~enable:vdd ~width:64 in
        ignore (x.value -- ("cmd_reg" ^ Int.to_string index) : Signal.t);
        Hashtbl.set t.table ~key:index ~data:x;
        x
      | Some x -> x
    ;;

    let lookup t index =
      match Hashtbl.find t.table index with
      | None -> raise_s [%message "Failed to lookup variable" (index : int)]
      | Some x -> x.value
    ;;
  end

  module Memory_control = struct
    let var_wire (n, b) =
      let v = Always.Variable.wire ~default:(zero b) in
      ignore (v.value -- n : Signal.t);
      v
    ;;

    module I = struct
      type 'a t =
        { read_available : 'a
        ; read_data : 'a [@bits dbits]
        ; write_complete : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { read : 'a
        ; read_address : 'a [@bits dbits]
        ; write : 'a
        ; write_data : 'a [@bits dbits]
        ; write_address : 'a [@bits dbits]
        }
      [@@deriving sexp_of, hardcaml]

      let var_wires () = map t ~f:var_wire
    end
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_in : 'a Memory_control.I.t [@rtlprefix "mi_"]
      ; program_in : 'a Memory_control.I.t [@rtlprefix "pi_"]
      ; stack_in : 'a Memory_control.I.t [@rtlprefix "si_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_out : 'a Memory_control.O.t [@rtlprefix "mo_"]
      ; program_out : 'a Memory_control.O.t [@rtlprefix "po_"]
      ; stack_out : 'a Memory_control.O.t [@rtlprefix "so_"]
      ; zinc_registers : 'a Zinc_register.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let compile st (i : _ I.t) =
    let reg_spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
    let cmd = State_poly.normalise (Statement.simplify st).cmd in
    let stack_out = Memory_control.O.var_wires () in
    let program_out = Memory_control.O.var_wires () in
    let memory_out = Memory_control.O.var_wires () in
    let select_memory = function
      | Machine.Cache.Stack -> i.stack_in, stack_out
      | Program -> i.program_in, program_out
      | Mem -> i.memory_in, memory_out
    in
    let command_registers = Command_register.create reg_spec in
    let zinc_registers = Zinc_register.create reg_spec in
    (* XXX I figure we will probaby need to pre-process the computation to work
       out the number of states we need. Either way, it's not this once we
       support Cond and Iter. *)
    let num_states = List.length cmd in
    let module States = struct
      type t = int [@@deriving sexp_of, compare]

      let all = List.range 0 (num_states + 1)
    end
    in
    let sm = Always.State_machine.create (module States) reg_spec ~enable:vdd in
    ignore (sm.current -- "state" : Signal.t);
    let compile state_number cmd =
      let notyet () = raise_s [%message "not yet"] in
      match cmd with
      | Get_reg (id, zinc_register) ->
        ( state_number
        , Always.
            [ Command_register.var command_registers id
              <-- Zinc_register.get zinc_registers zinc_register
            ; sm.set_next (state_number + 1)
            ] )
      | Set_reg (zinc_register, expr) ->
        ( state_number
        , Always.
            [ Zinc_register.var zinc_registers zinc_register
              <-- Expression.compile (Command_register.lookup command_registers) expr
            ; sm.set_next (state_number + 1)
            ] )
      | Get_mem (id, which_memory, address) ->
        let memory_in, memory_out = select_memory which_memory in
        ( state_number
        , Always.
            [ memory_out.read <-- vdd
            ; memory_out.read_address
              <-- Expression.compile (Command_register.lookup command_registers) address
            ; when_
                memory_in.read_available
                [ Command_register.var command_registers id <-- memory_in.read_data
                ; sm.set_next (state_number + 1)
                ]
            ] )
      | Set_mem (which_memory, address, data) ->
        let memory_in, memory_out = select_memory which_memory in
        ( state_number
        , Always.
            [ memory_out.write <-- vdd
            ; memory_out.write_address
              <-- Expression.compile (Command_register.lookup command_registers) address
            ; memory_out.write_data
              <-- Expression.compile (Command_register.lookup command_registers) data
            ; when_ memory_in.write_complete [ sm.set_next (state_number + 1) ]
            ] )
      | Cond _ -> notyet ()
      | Iter _ -> notyet ()
    in
    let states = List.mapi cmd ~f:compile in
    Always.(
      compile
        [ proc Zinc_register.(map zinc_registers ~f:(fun r -> r <-- r.value) |> to_list)
        ; proc
            Memory_control.O.(
              map memory_out ~f:(fun r -> r <-- zero (width r.value)) |> to_list)
        ; proc
            Memory_control.O.(
              map stack_out ~f:(fun r -> r <-- zero (width r.value)) |> to_list)
        ; proc
            Memory_control.O.(
              map program_out ~f:(fun r -> r <-- zero (width r.value)) |> to_list)
        ; sm.switch (states @ [ num_states, [] ])
        ]);
    { O.memory_out; program_out; stack_out; zinc_registers }
    |> O.map ~f:(fun o -> o.value)
  ;;
end
