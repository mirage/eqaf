external int : string -> int -> int = "%string_unsafe_get" [@@noalloc]
external int32 : string -> int -> int32 = "%caml_string_get32u"

let equal a b =
  let len = (min : int -> int -> int) (String.length a) (String.length b) in
  let res0 = ref 0l in
  let res1 = ref 0 in

  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to pred len1 do
    let i = i * 4 in
    res0 := Int32.logor !res0 (Int32.logxor (int32 a i) (int32 b i))
  done ;

  for i = 0 to pred len0 do
    let i = len1 * 4 + i in
    res1 := !res1 lor (int a i lxor int b i) ;
  done ;

  String.length a = String.length b && !res0 = 0l && !res1 = 0
