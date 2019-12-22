(* round robin arbiter *)
open Import

type 'a prefix = ('a -> 'a -> 'a) -> 'a list -> 'a list

module Make (B : Comb.S) = struct
  open B

  let arbiter ~prefix ~unmasked_req ~masked_req =
    let sel_masked =
      tree ~arity:4 ~f:(reduce ~f:( |: )) (bits_msb masked_req)
    in
    let req = mux2 sel_masked masked_req unmasked_req in
    let mask, req =
      let mask =
        concat_msb @@ List.rev @@ prefix ( |: ) (List.rev (bits_msb req))
      in
      let smask = sll mask 1 in
      (smask &: req, mask ^: smask)
    in
    (mask, req)
end

open Signal

let reg_spec = Reg_spec.create () ~clock ~clear

let arbiter ~prefix ~enable ~req =
  let module A = Make (Signal) in
  let wreq = width req in
  let next = wire wreq in
  let masked_req = reg reg_spec ~enable next in
  let next', gnt = A.arbiter ~prefix ~unmasked_req:req ~masked_req in
  next <== next';
  gnt

module Test = struct
  open Printf
  module B = Bits
  module Cs = Cyclesim
  module Waveform = Hardcaml_waveterm.Waveform

  let test ~prefix ~bits =
    let req = input "req" bits in
    let gnt = arbiter ~prefix ~enable:vdd ~req in
    let circ = Circuit.create_exn ~name:"arbiter" [ output "gnt" gnt ] in
    let sim = Cyclesim.create circ in
    let waves, sim = Waveform.create sim in

    let req, gnt, gntn =
      ( Cyclesim.in_port sim "req",
        Cyclesim.out_port ~clock_edge:Before sim "gnt",
        Cyclesim.out_port ~clock_edge:After sim "gnt" )
    in
    Cyclesim.reset sim;

    for _ = 0 to 0 do
      req := B.random ~width:bits;
      while B.to_int !req <> 0 do
        printf "req: %s\n" (B.to_string !req);
        (*S.cycle_comb0 sim;
        S.cycle_seq sim;
        Printf.printf "gnt: %s\n" (B.to_string !gnt);
        S.cycle_comb1 sim;*)
        Cyclesim.cycle sim;
        Printf.printf "gnt: %s [%s]\n" (B.to_string !gnt) (B.to_string !gntn);
        req := B.(!req ^: !gnt)
      done
    done;
    Waveform.print waves
end
