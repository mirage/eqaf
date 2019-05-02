type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let length x = Bigarray.Array1.dim x [@@inline]
external int : bigstring -> int -> int = "%caml_ba_ref_1" [@@noalloc]
external int32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"

let equal a b =
  let len = (min : int -> int -> int) (length a) (length b) in
  let res0 = ref 0l in
  let res1 = ref 0 in

  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to pred len1 do
    let i = i * 4 in
    res0 := Int32.logor !res0 (Int32.logxor (int32 a i) (int32 a i))
  done ;

  for i = 0 to pred len0 do
    let i = len1 * 4 + i in
    res1 := !res1 lor (int a i lxor int b i) ;
  done ;

  length a = length b && !res0 = 0l && !res1 = 0
