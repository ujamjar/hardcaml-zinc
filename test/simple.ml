(*let a = 1
let b = 2
let c = a + b
*)

(*
let () = output_string stdout "hello world!"
*)

(*
type t = A | B | C
let a = match A with A -> 0 | B -> 1 | C -> 2 
*)

(*
let d = 
  try
    output_string stdout "ok\n"
  with _ -> output_string stdout "exn\n"
*)

(*
let f = open_in "tmp"
*)

let s = Bytes.init 14 (fun i -> 'a') 
let () = Bytes.set s 3 'b'
let () = Bytes.set s 12 'c'

