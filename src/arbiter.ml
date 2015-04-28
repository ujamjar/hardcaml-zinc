(* round robin arbiter *)
open HardCaml

type 'a prefix = ('a -> 'a -> 'a) -> 'a list -> 'a list

module Make(B : Comb.S) = struct
  open B

  let arbiter ~prefix ~unmasked_req ~masked_req = 
    let sel_masked = tree 4 (reduce (|:)) (bits masked_req) in
    let req = mux2 sel_masked masked_req unmasked_req in
    let mask, req = 
      let mask = concat @@ List.rev @@ prefix (|:) (List.rev (bits req)) in
      let smask = sll mask 1 in
      smask &: req, mask ^: smask
    in
    mask, req

end

open Signal.Comb

module Seq = Signal.Make_seq(struct
  let reg_spec = Signal.Seq.r_sync
  let ram_spec = Signal.Seq.r_none
end)

let arbiter ~prefix ~e ~req = 
  let module A = Make(Signal.Comb) in 
  let wreq = width req in
  let next = wire wreq in
  let masked_req = Seq.reg ~e next in
  let next', gnt = A.arbiter ~prefix ~unmasked_req:req ~masked_req in
  next <== next';
  gnt

module Test = struct

  open Printf
  module B = Bits.Comb.IntbitsList
  module Cs = Cyclesim.Make(B)
  module S = Cyclesim.Api

  module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

  let test ~prefix ~bits = 
    let req = input "req" bits in
    let gnt = arbiter ~prefix ~e:vdd ~req in
    let circ = Circuit.make "arbiter" [ output "gnt" gnt ] in
    let sim = Cs.make circ in
    let sim, waves = Waveterm_sim.wrap sim in

    let req,gnt,gntn = S.in_port sim "req", S.out_port sim "gnt", S.out_port_next sim "gnt" in
    S.reset sim;
    
    for i=0 to 0 do
      req := B.srand bits;
      while B.to_int !req <> 0 do
        printf "req: %s\n" (B.to_string !req);
        (*S.cycle_comb0 sim;
        S.cycle_seq sim;
        Printf.printf "gnt: %s\n" (B.to_string !gnt);
        S.cycle_comb1 sim;*)
        S.cycle sim;
        Printf.printf "gnt: %s [%s]\n" (B.to_string !gnt) (B.to_string !gntn);
        req := B.(!req ^: !gnt);
      done
    done;
    Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

end


