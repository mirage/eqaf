external unsafe_string_get : string -> int -> int = "%string_unsafe_get"

let equal a b =
  let ln = String.length a in
  if ln = String.length b then (
    let rt = ref 0 in
    for i = 0 to ln - 1 do
      rt := !rt lor (unsafe_string_get a i lxor unsafe_string_get b i)
    done ;
    !rt = 0 )
  else false
