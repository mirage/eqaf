let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let ( <.> ) f g = fun x -> f (g x)
let identity x = x

type g = Prgm.g array
type machine =
  { registers : int64 array
  ; mutable flags : int64
  ; heap : Bigstringaf.t }

type flag =
  | CF | PF | AF | ZF | SF | TF | IF | DF | OF

let pf = Format.fprintf

let pp_flags ppf v =
  let ( & ) = Int64.logand in
  let ( >>> ) = Int64.shift_right in
  pf ppf "CF:%Ld, PF:%Ld, AF:%Ld, ZF:%Ld, SF:%Ld, TF:%Ld, IF:%Ld, DF:%Ld, OF:%Ld"
    ((v >>> 0) & 1L)
    ((v >>> 2) & 1L)
    ((v >>> 4) & 1L)
    ((v >>> 6) & 1L)
    ((v >>> 7) & 1L)
    ((v >>> 8) & 1L)
    ((v >>> 9) & 1L)
    ((v >>> 10) & 1L)
    ((v >>> 11) & 1L)

let set_bit ~bit value =
  let ( lor ) = Int64.logor in
  let ( lsl ) = Int64.shift_left in
  value lor (1L lsl bit)

let clear_bit ~bit value =
  let ( land ) = Int64.logand in
  let ( lnot ) = Int64.lognot in
  let ( lsl ) = Int64.shift_left in
  value land (lnot (1L lsl bit))

let set_bit ~bit value = function
  | true  -> set_bit ~bit value
  | false -> clear_bit ~bit value

let set_flag machine flag value =
  match flag with
  | CF -> machine.flags <- set_bit ~bit:0 machine.flags value
  | PF -> machine.flags <- set_bit ~bit:2 machine.flags value
  | AF -> machine.flags <- set_bit ~bit:4 machine.flags value
  | ZF -> machine.flags <- set_bit ~bit:6 machine.flags value
  | SF -> machine.flags <- set_bit ~bit:7 machine.flags value
  | TF -> machine.flags <- set_bit ~bit:8 machine.flags value
  | IF -> machine.flags <- set_bit ~bit:9 machine.flags value
  | DF -> machine.flags <- set_bit ~bit:10 machine.flags value
  | OF -> machine.flags <- set_bit ~bit:11 machine.flags value

let get_flag machine flag =
  let ( >>> ) = Int64.shift_right in
  let value = match flag with
    | CF -> machine.flags >>> 0
    | PF -> machine.flags >>> 2
    | AF -> machine.flags >>> 4
    | ZF -> machine.flags >>> 6
    | SF -> machine.flags >>> 7
    | TF -> machine.flags >>> 8
    | IF -> machine.flags >>> 9
    | DF -> machine.flags >>> 10
    | OF -> machine.flags >>> 11 in
  if Int64.logand value 0b1L = 0b0L then false else true

let (.?[]) heap (size, indice) = match size with
  | Size.Byte -> (Int64.of_int <.> Char.code) @@ Bigstringaf.get heap (Int64.to_int indice)
  | Size.Word -> Int64.of_int @@ Bigstringaf.get_int16_be heap (Int64.to_int indice)
  | Size.Long -> Int64.of_int32 @@ Bigstringaf.get_int32_be heap (Int64.to_int indice)
  | Size.Quad -> Bigstringaf.get_int64_be heap (Int64.to_int indice)

let (.?[]<-) heap (size, indice) v =
  let ( land ) = Int64.logand in
  match size with
  | Size.Byte -> Bigstringaf.set heap (Int64.to_int indice) ((Char.chr <.> Int64.to_int) (v land 0xffL))
  | Size.Word -> Bigstringaf.set_int16_be heap (Int64.to_int indice) (Int64.to_int (v land 0xffffL))
  | Size.Long -> Bigstringaf.set_int32_be heap (Int64.to_int indice) (Int64.to_int32 (v land 0xffffffffL))
  | Size.Quad -> Bigstringaf.set_int64_be heap (Int64.to_int indice) v

let mask size value =
  let ( land ) = Int64.logand in
  match size with
  | Size.Byte -> value land 0xffL
  | Size.Word -> value land 0xffffL
  | Size.Long -> value land 0xffffffffL
  | Size.Quad -> value

let load_direct machine = function
  | Address.Register { size; register; } ->
    machine.registers.Register.![size, register]
  | Address.Immediate { size; value; _ } ->
    mask size value
  | Address.Indirect { size; base; index; process; _ } ->
    let base  = Option.map Register.(fun reg -> machine.registers.%[reg]) base  in
    let index = Option.map Register.(fun reg -> machine.registers.%[reg]) index in
    mask size (process ?base ?index ())

let load_indirect machine = function
  | Address.Register { size; register; } ->
    let value = machine.registers.Register.![size, register] in
    machine.heap.?[size, value]
  | Address.Immediate { size; value; _ } ->
    machine.heap.?[size, value]
  | Address.Indirect { size; base; index; process; _ } ->
    let base  = Option.map Register.(fun reg -> machine.registers.%[reg]) base  in
    let index = Option.map Register.(fun reg -> machine.registers.%[reg]) index in
    machine.heap.?[size, process ?base ?index ()]

let load machine = function
  | Address.Register _ as address -> load_direct machine address
  | Address.Immediate _ as address -> load_direct machine address
  | Address.Indirect _ as address -> load_indirect machine address

let set machine ~f src dst =
  let src = load machine src in
  match dst with
  | Address.Register { size; register; } ->
    let dst = load_direct machine dst in
    machine.registers.Register.![size, register] <- f src dst
  | Address.Indirect { size; base; index; process; _ } ->
    let dst   = load_indirect machine dst in
    let base  = Option.map Register.(fun reg -> machine.registers.%[reg]) base  in
    let index = Option.map Register.(fun reg -> machine.registers.%[reg]) index in
    machine.heap.?[size, process ?base ?index ()] <- f src dst
  | Address.Immediate { size; value; _} ->
    let dst   = load_indirect machine dst in
    machine.heap.?[size, value] <- f src dst

let is_even x = Int64.rem x 2L = 0L

let instr_sar machine src dst =
  set machine ~f:(fun src dst -> Int64.shift_right dst (Int64.to_int src)) src dst

let instr_shr machine src dst =
  set machine ~f:(fun src dst -> Int64.shift_right_logical dst (Int64.to_int src)) src dst

let instr_add machine src dst =
  set machine ~f:(fun src dst -> Int64.add dst src) src dst ;
  let dst = load_direct machine dst in
  set_flag machine SF (dst < 0L) ;
  set_flag machine ZF (dst = 0L) ;
  set_flag machine PF (is_even dst)
(* TODO: CF (overflow unsigned) and OF (overflow signed) *)

let instr_and machine src dst =
  set machine ~f:(fun src dst -> Int64.logand dst src) src dst ;
  set_flag machine OF false ;
  set_flag machine CF false ;
  let dst = load_direct machine dst in
  set_flag machine SF (dst < 0L) ;
  set_flag machine ZF (dst = 0L) ;
  set_flag machine PF (is_even dst)

let instr_xor machine src dst =
  set machine ~f:(fun src dst -> Int64.logxor dst src) src dst ;
  set_flag machine OF false ;
  set_flag machine CF false ;
  let dst = load_direct machine dst in
  set_flag machine SF (dst < 0L) ;
  set_flag machine ZF (dst = 0L) ;
  set_flag machine PF (is_even dst)

let instr_sub machine src dst =
  set machine ~f:(fun src dst -> Int64.sub dst src) src dst ;
  let dst = load_direct machine dst in
  set_flag machine SF (dst < 0L) ;
  set_flag machine ZF (dst = 0L) ;
  set_flag machine PF (is_even dst)
(* TODO: CF (overflow unsigned) and OF (overflow signed) *)

let instr_cmp machine b a =
  let a = load machine a in
  let b = load machine b in
  let r = Int64.sub a b  in
  set_flag machine SF (r < 0L) ;
  set_flag machine ZF (r = 0L) ;
  set_flag machine PF (is_even r)

let instr_or machine src dst =
  set machine ~f:(fun src dst -> Int64.logor dst src) src dst ;
  set_flag machine OF false ;
  set_flag machine CF false ;
  let dst = load_direct machine dst in
  set_flag machine SF (dst < 0L) ;
  set_flag machine ZF (dst = 0L) ;
  set_flag machine PF (is_even dst)

let instr_movz machine size src dst =
  let src = load machine src in
  let value = let ( land ) = Int64.logand in match size with
    | Size.Byte -> src land 0xffL
    | Size.Word -> src land 0xffffL
    | Size.Long -> src land 0xffffffffL
    | Size.Quad -> src in
  set machine ~f:(fun src _ -> src) (Address.immediate ~size:(Address.size dst) value) dst

let instr_mov machine src dst =
  set machine ~f:(fun src _ -> src) src dst

let instr_lea machine src dst =
  let value = load_direct machine src in
  set machine ~f:(fun src _ -> src)
    (Address.Immediate { size= Address.size src; value; literal= None; }) dst

let instr_jmp machine value =
  let value = load_direct machine value in
  machine.registers.Register.%[Register.eip] <- value ;
  value

let instr_set_reference machine condition value =
  match condition, value with
  | (`E | `Z), Address.Register { size; register; } ->
    machine.registers.Register.![size, register] <- if get_flag machine ZF then 1L else 0L ;
  | (`E | `Z), Address.Immediate { size; value; _ } ->
    machine.heap.?[size, value] <- if get_flag machine ZF then 1L else 0L
  | (`E | `Z), Address.Indirect { size; base; index; process; _ } ->
    let base  = Option.map Register.(fun reg -> machine.registers.%[reg]) base  in
    let index = Option.map Register.(fun reg -> machine.registers.%[reg]) index in
    machine.heap.?[size, process ?base ?index ()] <- if get_flag machine ZF then 1L else 0L

let instr_short_jump machine condition value =
  let value = load_direct machine value in
  match condition with
  | `E ->
    if get_flag machine ZF then `Jump value else `Continue
  | `G ->
    if get_flag machine SF = get_flag machine OF && not (get_flag machine ZF)
    then `Jump value else `Continue
  | `L ->
    if get_flag machine SF <> get_flag machine OF
    then `Jump value else `Continue
  | `B ->
    if get_flag machine CF
    then `Jump value else `Continue
  | `NE ->
    if not (get_flag machine ZF) then `Jump value else `Continue
  | `GE ->
    if get_flag machine SF = get_flag machine OF || get_flag machine ZF
    then `Jump value else `Continue
  | `LE ->
    if get_flag machine SF <> get_flag machine OF || get_flag machine ZF
    then `Jump value else `Continue

let eval_instr machine = function
  | Instr.CMP (a, b) ->
    instr_cmp machine a b ;
    `Continue
  | Instr.MOV (src, dst) ->
    instr_mov machine src dst ;
    `Continue
  | Instr.JMP value ->
    let idx = instr_jmp machine value in
    `Jump idx
  | Instr.SHR (src, dst) ->
    instr_shr machine src dst ;
    `Continue
  | Instr.SAR (src, dst) ->
    instr_sar machine src dst ;
    `Continue
  | Instr.ADD (src, dst) ->
    instr_add machine src dst ;
    `Continue
  | Instr.XOR (src, dst) ->
    instr_xor machine src dst ;
    `Continue
  | Instr.SUB (src, dst) ->
    instr_sub machine src dst ;
    `Continue
  | Instr.OR  (src, dst) ->
    instr_or  machine src dst ;
    `Continue
  | Instr.AND (src, dst) ->
    instr_and machine src dst ;
    `Continue
  | Instr.LEA (src, dst) ->
    instr_lea machine src dst ;
    `Continue
  | Instr.J (c, v) ->
    instr_short_jump machine c v
  | Instr.MOVZ (size, src, dst) ->
    instr_movz machine size src dst ;
    `Continue
  | Instr.RET -> `Done
  | Instr.SET (condition, v) ->
    instr_set_reference machine condition v ;
    `Continue
  | _ -> assert false

let eval machine = function
  | Prgm.Instr instr ->
    eval_instr machine instr
  | Prgm.Label _ -> `Continue

let run symbol ?(registers= Array.make 20 0L) ~heap prgm =
  let machine = { registers; flags= 0L; heap; } in
  let rec go ticks =
    let eip = Int64.to_int machine.registers.Register.%[Register.eip] in
    match eval machine prgm.(eip) with
    | `Continue ->
      machine.registers.Register.%[Register.eip] <- Int64.(succ (of_int eip)) ; go (succ ticks)
    | `Jump eip ->
      machine.registers.Register.%[Register.eip] <- eip ; go (succ ticks)
    | `Done -> ticks in
  let eip = ref 0 in
  Array.iteri (fun idx -> function
      | Prgm.Label v -> if symbol = v then eip := idx
      | _ -> ()) prgm ;
  machine.registers.Register.%[Register.eip] <- Int64.of_int !eip ;
  go 0

let execute ~registers ~heap symbol prgm =
  let open Rresult in
  Prgm.extract symbol prgm
  >>| List.map (fun (_, (_, v)) -> v)
  >>| Array.concat >>| Array.to_list >>= fun prgm ->
  let prgm = Prgm.resolve prgm in
  let prgm = Array.of_list prgm in
  let ticks = run symbol ~registers ~heap:heap.Heap.heap prgm in Ok ticks
