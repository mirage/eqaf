type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val equal : bigstring -> bigstring -> bool
(** Constant time equal function on {!bigstring}. *)
