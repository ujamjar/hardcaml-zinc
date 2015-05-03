
(* say hello world *)
let () = output_string stdout "Hey! hello world from hardcamlzinc!\n"

(*let a = 1
let b = 2
let c = a + b
*)

(* switch instruction *)
type t = A | B | C
let a = match A with A -> 0 | B -> 1 | C -> 2 

(* exceptions *)
let () = 
  try
    output_string stdout "not raised\n"
  with _ -> output_string stdout "ooops\n"
let () = 
  try raise Not_found
  with Not_found -> output_string stdout "caught exn\n"

(* env vars *)
let x = Sys.getenv "PATH"
let () = print_endline x

(* args *)
let () = print_endline Sys.argv.(0)
let () = try print_endline Sys.argv.(1) with _ -> output_string stdout "no arg\n"

(* basic file I/O *)
let f = open_in "tmp.in"
let g = open_out "tmp.out"

let s = input_line f
let () = output_string g ("copied: " ^ s)

let () = close_in f
let () = close_out g

let () = output_string stdout "el finito!\n"

(* string ops *)
(*
let s = Bytes.init 14 (fun i -> 'a') 
let () = Bytes.set s 3 'b'
let () = Bytes.set s 12 'c'
*)
