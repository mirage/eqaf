let[@inline] get x i = String.unsafe_get x i |> Char.code

(* XXX(dinosaure): we use [unsafe_get] to avoid jump to exception:

        sarq    $1, %rbx
        movzbq  (%rax,%rbx), %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

external unsafe_get_int16 : string -> int -> int = "%caml_string_get16u"
let[@inline] get16 x i = unsafe_get_int16 x i

(* XXX(dinosaure): same as [unsafe_get] but for [int16]:

        sarq    $1, %rbx
        movzwq  (%rax,%rbx), %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

let equal ~ln a b =
  let l1 = ln asr 1 in

  (*
        sarq    $1, %rcx
        orq     $1, %rcx
  *)

  let r = ref 0 in

  (*
        movq    $1, %rdx
  *)

  for i = 0 to pred l1 do r := !r lor (get16 a (i * 2) lxor get16 b (i * 2)) done ;

  (*
        movq    $1, %rsi
        addq    $-2, %rcx
        cmpq    %rcx, %rsi
        jg      .L104
.L105:
        leaq    -1(%rsi,%rsi), %r8

        sarq    $1, %r8
        movzwq  (%rdi,%r8), %r9
        leaq    1(%r9,%r9), %r9
        movzwq  (%rbx,%r8), %r8
        leaq    1(%r8,%r8), %r8

     // [unsafe_get_int16 a i] and [unsafe_get_int6 b i]

        xorq    %r9, %r8
        orq     $1, %r8
        orq     %r8, %rdx
        movq    %rsi, %r8
        addq    $2, %rsi
        cmpq    %rcx, %r8
        jne     .L105
.L104:
  *)

  for _ = 1 to ln land 1 do r := !r lor (get a (ln - 1) lxor get b (ln - 1)) done ;

  (*
        movq    $3, %rsi
        movq    %rax, %rcx
        andq    $3, %rcx
        cmpq    %rcx, %rsi
        jg      .L102
.L103:
        movq    %rax, %r8
        addq    $-2, %r8

        sarq    $1, %r8
        movzbq  (%rdi,%r8), %r9
        leaq    1(%r9,%r9), %r9
        movzbq  (%rbx,%r8), %r8
        leaq    1(%r8,%r8), %r8

     // [unsafe_get a i] and [unsafe_get b i]

        xorq    %r9, %r8
        orq     $1, %r8
        orq     %r8, %rdx
        movq    %rsi, %r8
        addq    $2, %rsi
        cmpq    %rcx, %r8
        jne     .L103
.L102:
  *)

  !r = 0

(*
        cmpq    $1, %rdx
        sete    %al
        movzbq  %al, %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

let[@inline] min (a:int) b = if a < b then a else b
(* XXX(dinosaure): we should delete the branch, TODO! *)

let equal a b =
  let al = String.length a in
  let bl = String.length b in
  let ln = min al bl in
  if (al lxor ln) lor (bl lxor ln) <> 0
  then false
  else equal ~ln a b

let[@inline always] compare (a:int) b = a - b
let[@inline always] minus_one_or_less n = ((n land min_int) asr Sys.int_size) land 1
let[@inline always] one_if_not_zero n = (minus_one_or_less n) lor (minus_one_or_less (-n))

let compare_le ~ln a b =
  let r = ref 0 in
  let i = ref (pred ln) in

  while !i >= 0 do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    let n = minus_one_or_less c + one_if_not_zero c in
    let n = n lsr ((one_if_not_zero !r) lsl 1) in
    r := n + !r ;
    decr i ;
  done ;

  (!r land 1) - (!r lsr 1)

let compare_le_with_len ~len:ln a b =
  let al = String.length a in
  let bl = String.length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_le_with_len"
  else compare_le ~ln a b

let compare_le a b =
  let al = String.length a in
  let bl = String.length b in
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
    let n = minus_one_or_less c + one_if_not_zero c in
    let n = n lsr ((one_if_not_zero !r) lsl 1) in
    r := n + !r ;
    incr i ;
  done ;

  (!r land 1) - (!r lsr 1)

let compare_be_with_len ~len:ln a b =
  let al = String.length a in
  let bl = String.length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_be_with_len"
  else compare_be ~ln a b

let compare_be a b =
  let al = String.length a in
  let bl = String.length b in
  if al < bl then 1
  else if al > bl then (-1)
  else compare_be ~ln:al (* = bl *) a b
