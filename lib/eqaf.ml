external c_equal : string -> string -> bool = "eqaf" [@@noalloc]

external string_get : string -> int -> int = "%string_unsafe_get" [@@noalloc]

let ml_equal a b =
  let ln = String.length a in
  if ln = String.length b then (
    let rt = ref 0 in
    for i = 0 to ln - 1 do
      rt := !rt lor (string_get a i lxor string_get b i)
    done ;
    !rt = 0 )
  else false

module C = struct
  let[@inline] equal a b = c_equal a b
end

module OCaml = struct
  let[@inline] equal a b = ml_equal a b
end
