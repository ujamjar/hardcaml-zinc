module Make(M : Ops.S) : sig

  open M

  val val_int : t -> t
  val val_unit : t
  val val_false : t
  val val_true : t 
  val int_val : t -> t

  val is_int : t -> t
  val is_block : t -> t

  val make_header : t -> t -> t -> t

  val size : t -> t
  val colour : t -> t
  val tag : t -> t

  val black : t
  val gray : t
  val blue : t
  val white : t

  val lazy_tag : t
  val closure_tag : t
  val object_tag : t
  val infix_tag : t
  val forward_tag : t
  val no_scan_tag : t
  val abstract_tag : t
  val string_tag : t
  val double_tag : t
  val double_array_tag : t
  val custom_tag : t

end

