(* Load a byte code executable *)
open Base
open Hardcaml_zinc
open Expect_test_helpers_kernel

let%expect_test "load helloworld" =
  let bc = Load.bytecode_exe "../examples/helloworld.bc" in
  print_s [%message "Bytecode executable" (bc : Load.bytecode_exe)];
  [%expect{|
    ("Bytecode executable" (
      bc (
        (toc (
          (CODE 10568)
          (DLPT 0)
          (DLLS 0)
          (PRIM 7973)
          (DATA 604)
          (SYMB 352)
          (CRCS 1135)
          (DBUG 143696)))
        (crcs (
          (Stdlib__weak         ())
          (Stdlib__unit         ())
          (Stdlib__uchar        ())
          (Stdlib__sys          ())
          (Stdlib__stringLabels ())
          (Stdlib__string       ())
          (Stdlib__stream       ())
          (Stdlib__stdLabels    ())
          (Stdlib__stack        ())
          (Stdlib__spacetime    ())
          (Stdlib__set          ())
          (Stdlib__seq          ())
          (Stdlib__scanf        ())
          (Stdlib__result       ())
          (Stdlib__random       ())
          (Stdlib__queue        ())
          (Stdlib__printf       ())
          (Stdlib__printexc     ())
          (Stdlib__pervasives   ())
          (Stdlib__parsing      ())
          (Stdlib__option       ())
          (Stdlib__oo           ())
          (Stdlib__obj          ())
          (Stdlib__nativeint    ())
          (Stdlib__moreLabels   ())
          (Stdlib__marshal      ())
          (Stdlib__map          ())
          (Stdlib__listLabels   ())
          (Stdlib__list         ())
          (Stdlib__lexing       ())
          (Stdlib__lazy         ())
          (Stdlib__int64        ())
          (Stdlib__int32        ())
          (Stdlib__int          ())
          (Stdlib__hashtbl      ())
          (Stdlib__genlex       ())
          (Stdlib__gc           ())
          (Stdlib__fun          ())
          (Stdlib__format       ())
          (Stdlib__float        ())
          (Stdlib__filename     ())
          (Stdlib__ephemeron    ())
          (Stdlib__digest       ())
          (Stdlib__complex      ())
          (Stdlib__char         ())
          (Stdlib__callback     ())
          (Stdlib__bytesLabels  ())
          (Stdlib__bytes        ())
          (Stdlib__buffer       ())
          (Stdlib__bool         ())
          (Stdlib__bigarray     ())
          (Stdlib__arrayLabels  ())
          (Stdlib__array        ())
          (Stdlib__arg          ())
          (Stdlib                   (<opaque>))
          (Std_exit                 (<opaque>))
          (Dune__exe__Helloworld    (<opaque>))
          (CamlinternalFormatBasics (<opaque>))))
        (dplt "")
        (dlls "")
        (code <opaque>)
        (prim (
          caml_abs_float
          caml_acos_float
          caml_add_debug_info
          caml_add_float
          caml_alloc_dummy
          caml_alloc_dummy_float
          caml_alloc_dummy_function
          caml_alloc_dummy_infix
          caml_array_append
          caml_array_blit
          caml_array_concat
          caml_array_get
          caml_array_get_addr
          caml_array_get_float
          caml_array_set
          caml_array_set_addr
          caml_array_set_float
          caml_array_sub
          caml_array_unsafe_get
          caml_array_unsafe_get_float
          caml_array_unsafe_set
          caml_array_unsafe_set_addr
          caml_array_unsafe_set_float
          caml_asin_float
          caml_atan2_float
          caml_atan_float
          caml_ba_blit
          caml_ba_change_layout
          caml_ba_create
          caml_ba_dim
          caml_ba_dim_1
          caml_ba_dim_2
          caml_ba_dim_3
          caml_ba_fill
          caml_ba_get_1
          caml_ba_get_2
          caml_ba_get_3
          caml_ba_get_generic
          caml_ba_kind
          caml_ba_layout
          caml_ba_num_dims
          caml_ba_reshape
          caml_ba_set_1
          caml_ba_set_2
          caml_ba_set_3
          caml_ba_set_generic
          caml_ba_slice
          caml_ba_sub
          caml_ba_uint8_get16
          caml_ba_uint8_get32
          caml_ba_uint8_get64
          caml_ba_uint8_set16
          caml_ba_uint8_set32
          caml_ba_uint8_set64
          caml_backtrace_status
          caml_blit_bytes
          caml_blit_string
          caml_bswap16
          caml_bytes_compare
          caml_bytes_equal
          caml_bytes_get
          caml_bytes_get16
          caml_bytes_get32
          caml_bytes_get64
          caml_bytes_greaterequal
          caml_bytes_greaterthan
          caml_bytes_lessequal
          caml_bytes_lessthan
          caml_bytes_notequal
          caml_bytes_of_string
          caml_bytes_set
          caml_bytes_set16
          caml_bytes_set32
          caml_bytes_set64
          caml_ceil_float
          caml_channel_descriptor
          caml_classify_float
          caml_compare
          caml_convert_raw_backtrace
          caml_convert_raw_backtrace_slot
          caml_copysign_float
          caml_cos_float
          caml_cosh_float
          caml_create_bytes
          caml_create_string
          caml_div_float
          caml_dynlink_add_primitive
          caml_dynlink_close_lib
          caml_dynlink_get_current_libs
          caml_dynlink_lookup_symbol
          caml_dynlink_open_lib
          caml_ensure_stack_capacity
          caml_ephe_blit_data
          caml_ephe_blit_key
          caml_ephe_check_data
          caml_ephe_check_key
          caml_ephe_create
          caml_ephe_get_data
          caml_ephe_get_data_copy
          caml_ephe_get_key
          caml_ephe_get_key_copy
          caml_ephe_set_data
          caml_ephe_set_key
          caml_ephe_unset_data
          caml_ephe_unset_key
          caml_eq_float
          caml_equal
          caml_exp_float
          caml_expm1_float
          caml_fill_bytes
          caml_fill_string
          caml_final_register
          caml_final_register_called_without_value
          caml_final_release
          caml_float_compare
          caml_float_of_int
          caml_float_of_string
          caml_floatarray_create
          caml_floatarray_get
          caml_floatarray_set
          caml_floatarray_unsafe_get
          caml_floatarray_unsafe_set
          caml_floor_float
          caml_fma_float
          caml_fmod_float
          caml_format_float
          caml_format_int
          caml_fresh_oo_id
          caml_frexp_float
          caml_gc_compaction
          caml_gc_counters
          caml_gc_full_major
          caml_gc_get
          caml_gc_huge_fallback_count
          caml_gc_major
          caml_gc_major_slice
          caml_gc_minor
          caml_gc_minor_words
          caml_gc_quick_stat
          caml_gc_set
          caml_gc_stat
          caml_ge_float
          caml_get_current_callstack
          caml_get_current_environment
          caml_get_exception_backtrace
          caml_get_exception_raw_backtrace
          caml_get_global_data
          caml_get_major_bucket
          caml_get_major_credit
          caml_get_minor_free
          caml_get_public_method
          caml_get_section_table
          caml_greaterequal
          caml_greaterthan
          caml_gt_float
          caml_hash
          caml_hash_univ_param
          caml_hexstring_of_float
          caml_hypot_float
          caml_input_value
          caml_input_value_from_bytes
          caml_input_value_from_string
          caml_input_value_to_outside_heap
          caml_install_signal_handler
          caml_int32_add
          caml_int32_and
          caml_int32_bits_of_float
          caml_int32_bswap
          caml_int32_compare
          caml_int32_div
          caml_int32_float_of_bits
          caml_int32_format
          caml_int32_mod
          caml_int32_mul
          caml_int32_neg
          caml_int32_of_float
          caml_int32_of_int
          caml_int32_of_string
          caml_int32_or
          caml_int32_shift_left
          caml_int32_shift_right
          caml_int32_shift_right_unsigned
          caml_int32_sub
          caml_int32_to_float
          caml_int32_to_int
          caml_int32_xor
          caml_int64_add
          caml_int64_add_native
          caml_int64_and
          caml_int64_and_native
          caml_int64_bits_of_float
          caml_int64_bswap
          caml_int64_compare
          caml_int64_div
          caml_int64_div_native
          caml_int64_float_of_bits
          caml_int64_format
          caml_int64_mod
          caml_int64_mod_native
          caml_int64_mul
          caml_int64_mul_native
          caml_int64_neg
          caml_int64_neg_native
          caml_int64_of_float
          caml_int64_of_int
          caml_int64_of_int32
          caml_int64_of_nativeint
          caml_int64_of_string
          caml_int64_or
          caml_int64_or_native
          caml_int64_shift_left
          caml_int64_shift_right
          caml_int64_shift_right_unsigned
          caml_int64_sub
          caml_int64_sub_native
          caml_int64_to_float
          caml_int64_to_int
          caml_int64_to_int32
          caml_int64_to_nativeint
          caml_int64_xor
          caml_int64_xor_native
          caml_int_as_pointer
          caml_int_compare
          caml_int_of_float
          caml_int_of_string
          caml_invoke_traced_function
          caml_lazy_follow_forward
          caml_lazy_make_forward
          caml_ldexp_float
          caml_le_float
          caml_lessequal
          caml_lessthan
          caml_lex_engine
          caml_log10_float
          caml_log1p_float
          caml_log_float
          caml_lt_float
          caml_make_array
          caml_make_float_vect
          caml_make_vect
          caml_marshal_data_size
          caml_md5_chan
          caml_md5_string
          caml_ml_bytes_length
          caml_ml_channel_size
          caml_ml_channel_size_64
          caml_ml_close_channel
          caml_ml_enable_runtime_warnings
          caml_ml_flush
          caml_ml_flush_partial
          caml_ml_input
          caml_ml_input_char
          caml_ml_input_int
          caml_ml_input_scan_line
          caml_ml_open_descriptor_in
          caml_ml_open_descriptor_out
          caml_ml_out_channels_list
          caml_ml_output
          caml_ml_output_bytes
          caml_ml_output_char
          caml_ml_output_int
          caml_ml_output_partial
          caml_ml_pos_in
          caml_ml_pos_in_64
          caml_ml_pos_out
          caml_ml_pos_out_64
          caml_ml_runtime_warnings_enabled
          caml_ml_seek_in
          caml_ml_seek_in_64
          caml_ml_seek_out
          caml_ml_seek_out_64
          caml_ml_set_binary_mode
          caml_ml_set_channel_name
          caml_ml_string_length
          caml_modf_float
          caml_mul_float
          caml_nativeint_add
          caml_nativeint_and
          caml_nativeint_bswap
          caml_nativeint_compare
          caml_nativeint_div
          caml_nativeint_format
          caml_nativeint_mod
          caml_nativeint_mul
          caml_nativeint_neg
          caml_nativeint_of_float
          caml_nativeint_of_int
          caml_nativeint_of_int32
          caml_nativeint_of_string
          caml_nativeint_or
          caml_nativeint_shift_left
          caml_nativeint_shift_right
          caml_nativeint_shift_right_unsigned
          caml_nativeint_sub
          caml_nativeint_to_float
          caml_nativeint_to_int
          caml_nativeint_to_int32
          caml_nativeint_xor
          caml_neg_float
          caml_neq_float
          caml_new_lex_engine
          caml_nextafter_float
          caml_notequal
          caml_obj_add_offset
          caml_obj_block
          caml_obj_dup
          caml_obj_is_block
          caml_obj_make_forward
          caml_obj_reachable_words
          caml_obj_set_tag
          caml_obj_tag
          caml_obj_truncate
          caml_obj_with_tag
          caml_output_value
          caml_output_value_to_buffer
          caml_output_value_to_bytes
          caml_output_value_to_string
          caml_parse_engine
          caml_power_float
          caml_raw_backtrace_length
          caml_raw_backtrace_next_slot
          caml_raw_backtrace_slot
          caml_realloc_global
          caml_record_backtrace
          caml_register_channel_for_spacetime
          caml_register_code_fragment
          caml_register_named_value
          caml_reify_bytecode
          caml_remove_debug_info
          caml_reset_afl_instrumentation
          caml_restore_raw_backtrace
          caml_round_float
          caml_runtime_parameters
          caml_runtime_variant
          caml_set_oo_id
          caml_set_parser_trace
          caml_setup_afl
          caml_signbit
          caml_signbit_float
          caml_sin_float
          caml_sinh_float
          caml_spacetime_enabled
          caml_spacetime_only_works_for_native_code
          caml_sqrt_float
          caml_static_alloc
          caml_static_free
          caml_static_release_bytecode
          caml_static_resize
          caml_string_compare
          caml_string_equal
          caml_string_get
          caml_string_get16
          caml_string_get32
          caml_string_get64
          caml_string_greaterequal
          caml_string_greaterthan
          caml_string_lessequal
          caml_string_lessthan
          caml_string_notequal
          caml_string_of_bytes
          caml_string_set
          caml_sub_float
          caml_sys_argv
          caml_sys_chdir
          caml_sys_close
          caml_sys_const_backend_type
          caml_sys_const_big_endian
          caml_sys_const_int_size
          caml_sys_const_max_wosize
          caml_sys_const_ostype_cygwin
          caml_sys_const_ostype_unix
          caml_sys_const_ostype_win32
          caml_sys_const_word_size
          caml_sys_executable_name
          caml_sys_exit
          caml_sys_file_exists
          caml_sys_get_argv
          caml_sys_get_config
          caml_sys_getcwd
          caml_sys_getenv
          caml_sys_is_directory
          caml_sys_isatty
          caml_sys_modify_argv
          caml_sys_open
          caml_sys_random_seed
          caml_sys_read_directory
          caml_sys_remove
          caml_sys_rename
          caml_sys_system_command
          caml_sys_time
          caml_sys_time_include_children
          caml_sys_unsafe_getenv
          caml_tan_float
          caml_tanh_float
          caml_terminfo_rows
          caml_trunc_float
          caml_update_dummy
          caml_weak_blit
          caml_weak_check
          caml_weak_create
          caml_weak_get
          caml_weak_get_copy
          caml_weak_set))
        (data
         "\132\149\166\190\000\000\002H\000\000\000B\000\000\001&\000\000\000\239\b\000\000\196\000\b\000\000\b\248-Out_of_memory\000\255\b\000\000\b\248)Sys_error\000\254\b\000\000\b\248'Failure\000\253\b\000\000\b\2480Invalid_argument\000\252\b\000\000\b\248+End_of_file\000\251\b\000\000\b\2480Division_by_zero\000\250\b\000\000\b\248)Not_found\000\249\b\000\000\b\248-Match_failure\000\248\b\000\000\b\248.Stack_overflow\000\247\b\000\000\b\248.Sys_blocked_io\000\246\b\000\000\b\248.Assert_failure\000\245\b\000\000\b\248:Undefined_recursive_module\000\244@\"%,,really_input%input\160@\160F@\160@\160G@0output_substring&output\160A\160C\160D\160F@\160A\160C\160D\160G@%%.12g!.\"%d%false$true\144A\144@%false$true.bool_of_string$true%false+char_of_int3index out of bounds<Pervasives.array_bound_error+Stdlib.Exit\025_j\000\127\240\000\000\000\000\000\000\025_j\000\255\240\000\000\000\000\000\000\025_j\000\127\240\000\000\000\000\000\001\025_j\000\127\239\255\255\255\255\255\255\025_j\000\000\016\000\000\000\000\000\000\025_j\000<\176\000\000\000\000\000\0005Pervasives.do_at_exit@,Hello world!@@")))) |}]
