open Base
open Hardcaml_zinc
module Sequential = Compile_hardware.Sequential

let top =
  ( Hardcaml_waveterm.Display_rules.(
      Rule.
        [ Sequential.I.(
            map port_names ~f:(fun n -> port_name_is n ~wave_format:(Bit_or Unsigned_int))
            |> to_list)
        ; Sequential.O.(
            map port_names ~f:(fun n -> port_name_is n ~wave_format:(Bit_or Unsigned_int))
            |> to_list)
        ; [ port_name_is "state" ~wave_format:Unsigned_int ]
        ]
      |> List.concat
      |> of_list)
  , 80 )
;;

let create instr =
  let uses = Compile_hardware.Statement.Usage.create instr in
  let zinc_registers =
    Set.union
      (Set.of_list (module Machine.Register) uses.read_registers)
      (Set.of_list (module Machine.Register) uses.write_registers)
    |> Set.to_list
  in
  let open Hardcaml_waveterm.Display_rules in
  let write_memory prefix =
    [ Rule.port_name_is (prefix ^ "o_write") ~wave_format:Bit
    ; Rule.port_name_is (prefix ^ "i_write_complete") ~wave_format:Bit
    ; Rule.port_name_is (prefix ^ "o_write_data") ~wave_format:Unsigned_int
    ; Rule.port_name_is (prefix ^ "o_write_address") ~wave_format:Unsigned_int
    ]
  in
  let read_memory prefix =
    [ Rule.port_name_is (prefix ^ "o_read") ~wave_format:Bit
    ; Rule.port_name_is (prefix ^ "i_read_available") ~wave_format:Bit
    ; Rule.port_name_is (prefix ^ "i_read_data") ~wave_format:Unsigned_int
    ; Rule.port_name_is (prefix ^ "o_read_address") ~wave_format:Unsigned_int
    ]
  in
  let write_memory typ prefix =
    if List.mem uses.write_memories typ ~equal:Machine.Cache.equal
    then write_memory prefix
    else []
  in
  let read_memory typ prefix =
    if List.mem uses.read_memories typ ~equal:Machine.Cache.equal
    then read_memory prefix
    else []
  in
  let display_height =
    7
    + (List.length zinc_registers * 3)
    + (List.length uses.write_memories * 10)
    + (List.length uses.read_memories * 10)
  in
  ( [ [ Rule.port_name_is "clock" ~wave_format:Bit
      ; Rule.port_name_is "state" ~wave_format:Unsigned_int
      ]
    ; List.map zinc_registers ~f:(fun r ->
          Rule.port_name_is
            (Machine.Register.sexp_of_t r |> Sexp.to_string_hum)
            ~wave_format:Unsigned_int)
    ; write_memory Mem "m"
    ; read_memory Mem "m"
    ; write_memory Program "p"
    ; read_memory Program "p"
    ; write_memory Stack "s"
    ; read_memory Stack "s"
    ]
    |> List.concat
    |> of_list
  , display_height )
;;
