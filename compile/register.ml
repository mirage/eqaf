let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type t =
  { name : string
  ; idx  : int
  ; size : Size.t }

let eip = { name= "eip"; idx= 0; size= Size.Quad }

let is_register = function
  | "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi"
  | "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
  | "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di"
  | "ah" | "al" | "ch" | "cl" | "dh" | "dl" | "bh" | "bl" | "spl" | "bpl" | "sil" | "dil" -> true
  | reg ->
    try let () = Scanf.sscanf reg "r%d%!"  ignore in true
    with _ ->
    try let () = Scanf.sscanf reg "r%dd%!" ignore in true
    with _ ->
    try let () = Scanf.sscanf reg "r%dw%!" ignore in true
    with _ ->
    try let () = Scanf.sscanf reg "r%db%!" ignore in true
    with _ -> false


let size = function
  | "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi" -> Size.Quad
  | "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi" -> Size.Long
  | "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di" -> Size.Word
  | "ah" | "al" | "ch" | "cl" | "dh" | "dl" | "bh" | "bl" | "spl" | "bpl" | "sil" | "dil" -> Size.Byte
  | reg ->
    try let () = Scanf.sscanf reg "r%d%!"  ignore in Size.Quad
    with _ ->
    try let () = Scanf.sscanf reg "r%dd%!" ignore in Size.Long
    with _ ->
    try let () = Scanf.sscanf reg "r%dw%!" ignore in Size.Word
    with _ ->
    try let () = Scanf.sscanf reg "r%db%!" ignore in Size.Byte
    with _ -> assert false

let idx = function
  | "rax" | "eax" | "ax" | "ah" | "al" -> 0
  | "rcx" | "ecx" | "cx" | "ch" | "cl" -> 1
  | "rbx" | "ebx" | "bx" | "bh" | "bl" -> 2
  | "rdx" | "edx" | "dx" | "dh" | "dl" -> 3
  | "rsp" | "esp" | "sp" | "spl" -> 4
  | "rbp" | "ebp" | "bp" | "bpl" -> 5
  | "rsi" | "esi" | "si" | "sil" -> 6
  | "rdi" | "edi" | "di" | "dil" -> 7
  | reg ->
    let res = ref 0 in
    Scanf.sscanf reg "r%d" (fun v -> res := v) ; 8 + !res

let idx reg = idx reg + 1 (* %eip *)

let pf = Format.fprintf
let pp : Format.formatter -> t -> unit = fun ppf { name; _ } -> pf ppf "%%%s" name

let v name =
  if not (is_register name)
  then invalid_arg "Invalid register %%%s" name
  else let size = size name in
    { name; size; idx= idx name; }

let rsi = v "rsi"
let rax = v "rax"
let rbx = v "rbx"
let rdi = v "rdi"

let size { size; _ } = size

let (.%[])
  : int64 array -> t -> int64
  = fun arr { name; idx; size; } ->
  let ( land ) = Int64.logand
  and ( lsr ) = Int64.shift_right in
  match name, size with
  | ("ah" | "ch" | "dh" | "bh"), Byte -> (arr.(idx) lsr 8) land 0xffL
  | _, Byte -> arr.(idx) land 0xffL
  | _, Word -> arr.(idx) land 0xffffL
  | _, Long -> arr.(idx) land 0xffffffffL
  | _, Quad -> arr.(idx)

let (.%[]<-)
  : int64 array -> t -> int64 -> unit
  = fun arr { name; idx; size; } v ->
  let ( land ) = Int64.logand in
  let ( lnot ) = Int64.lognot in
  let ( lor ) = Int64.logor in
  let ( lsl ) = Int64.shift_left in
  match name, size with
  | ("ah" | "ch" | "dh" | "bh"), Byte ->
    arr.(idx) <- arr.(idx) lor ((v land 0xffL) lsl 8)
  | _ ->
    let mask = Size.mask size in
    arr.(idx) <- (arr.(idx) land (lnot mask)) lor (v land mask)

let downcast : Size.t -> Size.t -> int64 -> int64
  = fun sa sb v ->
    let ( land ) = Int64.logand in
    match sa, sb with
    | Size.Quad, Size.Quad -> v
    | Size.Long, Size.Long -> v
    | Size.Word, Size.Word -> v
    | Size.Byte, Size.Byte -> v
    | Size.Byte, Size.(Quad | Long | Word)
    | Size.Word, Size.(Quad | Long)
    | Size.Long, Size.Quad -> assert false
    | _, Size.Long -> v land 0xffffffffL
    | _, Size.Word -> v land 0xffffL
    | _, Size.Byte -> v land 0xffL

let (.![])
  : int64 array -> (Size.t * t) -> int64
  = fun arr (size, t) ->
    if Size.equal size t.size then arr.%[t]
    else if Size.(size < t.size)
    then downcast t.size size arr.%[t]
    else invalid_arg "Impossible to promote value of %a from %a to %a"
        pp t Size.pp t.size Size.pp size

let (.![]<-)
  : int64 array -> (Size.t * t) -> int64 -> unit
  = fun arr (size, t) v ->
    if Size.equal size t.size then arr.%[t] <- v
    else if Size.(size < t.size)
    then arr.%[t] <- downcast t.size size v
    else invalid_arg "Impossible to promote value of %a from %a to %a"
        pp t Size.pp t.size Size.pp size
