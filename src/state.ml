(* XXX MERGE BACK INTO HARDCAML *)

open HardCaml
open Signal
open Comb

type state_var = Guarded.variable

type 'a statemachine =
  {
    reg : unit -> Guarded.variable;
    const : 'a -> Comb.t;
    var : Guarded.variable;
    cur : Comb.t;
    set : Comb.t -> Guarded.statement;
    next : 'a -> Guarded.statement;
    machine : ('a * Guarded.statements) list -> Guarded.statement;
    is : 'a -> Comb.t;
  }

(* new statemchine implementation *)
let statemachine_binary rspec enable states = 
  let open Guarded in
  (* assign a value to each state *)
  let nstates = List.length states in
  let ls = Utils.clog2 nstates in
  let states = Utils.mapi (fun i s -> s, consti ls i) states in
  (* state variable *)
  let reg () = g_reg rspec enable ls in 
  let state_var = reg () in 
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> failwith "couldn't find state"
  in
  let set s = state_var $== s in
  let next s = set (state_val s) in
  let machine cases = 
    g_switch (state_var#q) 
      (List.map (fun (s, c) -> state_val s, c) cases)
  in
  let is s = state_val s ==: state_var#q in
  let cur = state_var#q in
  { reg; const=state_val; var=state_var; cur; set; next; machine; is }

let statemachine_onehot rspec enable states = 
  let open Guarded in
  let nstates = List.length states in
  let onehot i = 
    let module B = Bits.Comb.IntbitsList in
    let ls = Utils.clog2 nstates in
    constibl B.(select (binary_to_onehot (consti ls i)) (nstates-1) 0)
  in
  let states = Utils.mapi 
    (fun i s -> s, (i, onehot i)) states 
  in
  let reg () = 
    g_reg 
      Types.({ rspec with (* must be reset to get into state 0 *)
        reg_clear_value = one nstates;
        reg_reset_value = one nstates; })
    enable nstates in 
  let state_var = reg () in
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> failwith "couldn't find state"
  in
  let set s = state_var $== s in
  let next s = set (snd (state_val s)) in
  let machine cases = 
    g_proc
      (List.map (fun (s, c) ->
        let i, _ = state_val s in
        g_when (bit state_var#q i) c) cases)
  in
  let is s = bit state_var#q (fst (state_val s)) in
  let signal = state_var#q in
  let cur = state_var#q in
  { reg; const=(fun s -> snd (state_val s)); var=state_var; cur; set; next; machine; is }

let statemachine_gray rspec enable states = 
  let open Guarded in
  (* assign a value to each state *)
  let nstates = List.length states in
  let ls = Utils.clog2 nstates in
  let gray i = 
    let module B = Bits.Comb.IntbitsList in
    constibl (B.binary_to_gray (B.consti ls i))
  in
  let states = Utils.mapi (fun i s -> s, gray i) states in
  (* state variable *)
  let reg () = g_reg rspec enable ls in 
  let state_var = reg () in
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> failwith "couldn't find state"
  in
  let set s = state_var $== s in
  let next s = set (state_val s) in
  let machine cases = 
    g_switch state_var#q 
      (List.map (fun (s, c) -> state_val s, c) cases)
  in
  let is s = state_val s ==: state_var#q in
  let signal = state_var#q in
  let cur = state_var#q in
  { reg; const=state_val; var=state_var; cur; set; next; machine; is }

let statemachine ?(encoding=`binary) = 
  match encoding with
  | `binary -> statemachine_binary
  | `onehot -> statemachine_onehot
  | `gray -> statemachine_gray 

module Seq(S : Seq_spec) = struct

  let make_spec 
    ?clk ?clkl 
    ?r ?rl ?rv 
    ?c ?cl ?cv 
    ?ge spec =
    let sel a b = 
      match a with
      | None -> b
      | Some(x) -> x
    in
    Types.({
      reg_clock       = sel clk  spec.reg_clock;
      reg_clock_level = sel clkl spec.reg_clock_level;
      reg_reset       = sel r    spec.reg_reset;
      reg_reset_level = sel rl   spec.reg_reset_level;
      reg_reset_value = sel rv   spec.reg_reset_value;
      reg_clear       = sel c    spec.reg_clear;
      reg_clear_level = sel cl   spec.reg_clear_level;
      reg_clear_value = sel cv   spec.reg_clear_value;
      reg_enable      = sel ge   spec.reg_enable;
    })

  let statemachine
    ?clk ?clkl 
    ?r ?rl ?rv 
    ?c ?cl ?cv 
    ?ge ?encoding ~e states =
    let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
    statemachine ?encoding spec e states

end

