(* print some [Sys] module values as we add them to the simulated runtime. *)

let () =
  print_string "backend_type ";
  print_endline
    (match Sys.backend_type with
    | Bytecode -> "bytecode"
    | Native -> "native"
    | Other s -> "other - " ^ s)
;;

let () =
  print_string "word_size ";
  print_endline (string_of_int Sys.word_size)
;;

let () =
  print_string "int_size ";
  print_endline (string_of_int Sys.int_size)
;;
