module Make(M : Ops.S) = struct

  open M

  let val_int x = (sll x 1) |: one
  let val_unit = one
  let val_false = one
  let val_true = const 3
  let int_val x = sra x 1

  let is_int v = v &: one
  let is_block v = (~: v) &: one

  let make_header size colour tag = 
    (sll (tag    &: const 255)  0) |:
    (sll (colour &: const   3)  8) |:
    (sll  size                 10) 

  let size hdr = srl hdr 10
  let colour hdr = (srl hdr 8) &: (const 3)
  let tag hdr = hdr &: (const 255)

  let white = const 0
  let gray = const 1
  let blue = const 2
  let black = const 3

  let lazy_tag = const Obj.lazy_tag
  let closure_tag = const Obj.closure_tag
  let object_tag = const Obj.object_tag
  let infix_tag = const Obj.infix_tag
  let forward_tag = const Obj.forward_tag
  let no_scan_tag = const Obj.no_scan_tag
  let abstract_tag = const Obj.abstract_tag
  let string_tag = const Obj.string_tag
  let double_tag = const Obj.double_tag
  let double_array_tag = const Obj.double_array_tag
  let custom_tag = const Obj.custom_tag

end

