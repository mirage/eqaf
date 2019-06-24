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

let[@inline always] compare (a:int) b = a - b
let[@inline always] sixteen_if_minus_one_or_less n = (n asr Sys.int_size) land 16
let[@inline always] eight_if_one_or_more n = ((-n) asr Sys.int_size) land 8

let compare_le ~ln a b =
  let r = ref 0 in
  let i = ref (pred ln) in

  while !i >= 0 do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    decr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_le_with_len ~len:ln a b =
  let al = length a in
  let bl = length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_le_with_len"
  else compare_le ~ln a b

let compare_le a b =
  let al = length a in
  let bl = length b in
  if al < bl
  then 1
  else if al > bl
  then (-1)
  else compare_le ~ln:al (* = bl *) a b

let compare_be ~ln a b =
  let r = ref 0 in
  let i = ref 0 in

  while !i < ln do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    incr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_be_with_len ~len:ln a b =
  let al = length a in
  let bl = length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_be_with_len"
  else compare_be ~ln a b

let compare_be a b =
  let al = length a in
  let bl = length b in
  if al < bl then 1
  else if al > bl then (-1)
  else compare_be ~ln:al (* = bl *) a b
