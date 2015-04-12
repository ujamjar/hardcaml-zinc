open HardCaml
open Signal

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

val statemachine : ?encoding:[`binary|`gray|`onehot] -> 
  Types.register -> Comb.t -> 
  'a list -> 'a statemachine

module Seq(S : Seq_spec) : sig
  open Signal.Types
  val statemachine : 
    ?clk:signal -> ?clkl:signal ->
    ?r:signal -> ?rl:signal -> ?rv:signal ->
    ?c:signal -> ?cl:signal -> ?cv:signal ->
    ?ge:signal -> ?encoding:[`binary|`gray|`onehot] -> 
    e:signal -> 'a list -> 'a statemachine
end
