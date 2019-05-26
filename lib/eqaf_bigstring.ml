type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let length x = Bigarray.Array1.dim x [@@inline]
let get x i = Bigarray.Array1.unsafe_get x i |> Char.code [@@inline]
external unsafe_get_int16 : bigstring -> int -> int = "%caml_bigstring_get16u" [@@noalloc]
let get16 x i = unsafe_get_int16 x i [@@inline]

let equal ~ln a b =
  let l1 = ln asr 1 in
  let r = ref 0 in
  for i = 0 to pred l1 do r := !r lor (get16 a (i * 2) lxor get16 b (i * 2)) done ;
  for _ = 1 to ln land 1 do r := !r lor (get a (ln - 1) lxor get b (ln - 1)) done ;
  !r = 0

let[@inline] min (a:int) b = if a < b then a else b

let equal a b =
  let al = length a in
  let bl = length b in
  let ln = min al bl in
  if (al lxor ln) lor (bl lxor ln) <> 0
  then false
  else equal ~ln a b
