
(* say hello world *)
let () = output_string stderr "Hey! hello world from hardcamlzinc!\n"

(*let a = 1
let b = 2
let c = a + b
*)

type foo = X of int | Y of (int * int) | Z

let x = List.fold_left (fun a -> function X i -> a+i | Y(i,j) -> a+i+j | Z -> a+1) 
  0 [ X 1; Y(1,2); Z; Z ]

(* switch instruction *)
type t = A | B | C
let a = match A with A -> 0 | B -> 1 | C -> 2 

(* exceptions *)
let () = 
  try
    output_string stderr "not raised\n"
  with _ -> output_string stderr "ooops\n"
let () = 
  try raise Not_found
  with Not_found -> output_string stderr "caught exn\n"

(* env vars *)
let x = Sys.getenv "PATH"
let () = prerr_endline x

(* args *)
let () = prerr_endline Sys.argv.(0)
let () = try prerr_endline Sys.argv.(1) with _ -> output_string stderr "no arg\n"

(* basic file I/O *)
let f = open_in "tmp.in"
let g = open_out "tmp.out"

let s = input_line f
let () = output_string g ("copied: " ^ s)

let () = close_in f
let () = close_out g

(* closure stuff *)

let n = 11
let rec f i = 1 + if i<0 then n else g (i-1)
and g i = f (i-1) + f i
let n = prerr_endline (string_of_int (f 3))

let () = output_string stderr "el finito!\n"

(* string ops *)
(*
let s = Bytes.init 14 (fun i -> 'a') 
let () = Bytes.set s 3 'b'
let () = Bytes.set s 12 'c'
*)
