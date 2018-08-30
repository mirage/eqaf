(** Constant time equal function on [string] in C. *)
module C : sig
  val equal : string -> string -> bool
end

(** Constant time equal function on [string] in OCaml. *)
module OCaml : sig
  val equal : string -> string -> bool
end
