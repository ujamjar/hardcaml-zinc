open Hardcaml
open Signal

type 'a statemachine = {
  reg : unit -> Always.Variable.t;
  const : 'a -> t;
  var : Always.Variable.t;
  cur : t;
  set : t -> Always.t;
  next : 'a -> Always.t;
  machine : ('a * Always.t) list -> Always.t;
  is : 'a -> t;
}

(* XXX this is deprecated and in Hardcaml proper now*)
(* val statemachine :
 *   ?encoding:[ `binary | `gray | `onehot ] ->
 *   Reg_spec.t ->
 *   t ->
 *   'a list ->
 *   'a statemachine
 * 
 * val statemachine :
 *   ?clk:signal ->
 *   ?clkl:signal ->
 *   ?r:signal ->
 *   ?rl:signal ->
 *   ?rv:signal ->
 *   ?c:signal ->
 *   ?cl:signal ->
 *   ?cv:signal ->
 *   ?ge:signal ->
 *   ?encoding:[ `binary | `gray | `onehot ] ->
 *   e:signal ->
 *   'a list ->
 *   'a statemachine *)
