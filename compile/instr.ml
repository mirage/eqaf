let () = Printexc.record_backtrace true

let ( <.> ) f g = fun x -> f (g x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let strf fmt = Format.asprintf fmt
let pf = Format.fprintf

let pp_option pp_val ppf = function
  | Some x -> pp_val ppf x
  | None -> ()

type unresolved =
  | MOVZ   of Size.t * Size.t option * Address.unresolved * Address.unresolved
  | MOVABS of Size.t option * Address.unresolved * Address.unresolved

  | MOV    of Size.t option * Address.unresolved * Address.unresolved
  | SAR    of Size.t option * Address.unresolved * Address.unresolved
  | LEA    of Size.t option * Address.unresolved * Address.unresolved
  | ADD    of Size.t option * Address.unresolved * Address.unresolved
  | AND    of Size.t option * Address.unresolved * Address.unresolved
  | XOR    of Size.t option * Address.unresolved * Address.unresolved
  | CMP    of Size.t option * Address.unresolved * Address.unresolved
  | SHR    of Size.t option * Address.unresolved * Address.unresolved
  | SUB    of Size.t option * Address.unresolved * Address.unresolved
  | OR     of Size.t option * Address.unresolved * Address.unresolved

  | SET    of char * Address.unresolved

  | INC    of Size.t option * Address.unresolved

  | JE     of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JB     of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JG     of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JL     of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JNE    of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JGE    of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JLE    of [ `Literal of string * string option | `Address of Address.unresolved ]
  | JMP    of [ `Literal of string * string option | `Address of Address.unresolved ]

  | CALL   of [ `Literal of string * string option | `Address of bool * Address.unresolved ]
  | RET

let pp_unresolved_instr ppf = function
  | MOVZ (size, sr, _, _) -> pf ppf "movz%a%a" Size.pp size (pp_option Size.pp) sr
  | MOVABS (size, _, _) -> pf ppf "movabs%a" (pp_option Size.pp) size
  | MOV (size, _, _) -> pf ppf "mov%a" (pp_option Size.pp) size
  | SAR (size, _, _) -> pf ppf "sar%a" (pp_option Size.pp) size
  | LEA (size, _, _) -> pf ppf "lea%a" (pp_option Size.pp) size
  | ADD (size, _, _) -> pf ppf "add%a" (pp_option Size.pp) size
  | XOR (size, _, _) -> pf ppf "xor%a" (pp_option Size.pp) size
  | CMP (size, _, _) -> pf ppf "cmp%a" (pp_option Size.pp) size
  | SHR (size, _, _) -> pf ppf "shr%a" (pp_option Size.pp) size
  | SUB (size, _, _) -> pf ppf "sub%a" (pp_option Size.pp) size
  | AND (size, _, _) -> pf ppf "and%a" (pp_option Size.pp) size
  | OR (size, _, _) -> pf ppf "or%a" (pp_option Size.pp) size
  | SET (flag, _) -> pf ppf "set%c" flag
  | INC (size, _) -> pf ppf "inc%a" (pp_option Size.pp) size
  | JE _ -> pf ppf "je"
  | JB _ -> pf ppf "jb"
  | JG _ -> pf ppf "jg"
  | JL _ -> pf ppf "jl"
  | JNE _ -> pf ppf "jne"
  | JGE _ -> pf ppf "jge"
  | JLE _ -> pf ppf "jle"
  | JMP _ -> pf ppf "jmp"
  | CALL _ -> pf ppf "call"
  | RET -> pf ppf "ret"

let collect ~tbl = function
  | MOVZ (_, _, a, b)
  | MOVABS (_, a, b)
  | MOV (_, a, b)
  | SAR (_, a, b)
  | LEA (_, a, b)
  | ADD (_, a, b)
  | AND (_, a, b)
  | XOR (_, a, b)
  | CMP (_, a, b)
  | SHR (_, a, b)
  | SUB (_, a, b)
  | OR (_, a, b) ->
    Address.collect ~tbl a ;
    Address.collect ~tbl b
  | SET (_, x) | INC (_, x) ->
    Address.collect ~tbl x
  | JE (`Literal (name, _))
  | JG (`Literal (name, _))
  | JB (`Literal (name, _))
  | JL (`Literal (name, _))
  | JNE (`Literal (name, _))
  | JGE (`Literal (name, _))
  | JLE (`Literal (name, _))
  | JMP (`Literal (name, _))
  | CALL (`Literal (name, _)) ->
    Hashtbl.add tbl name ()
  | JE (`Address v)
  | JG (`Address v)
  | JB (`Address v)
  | JL (`Address v)
  | JNE (`Address v)
  | JGE (`Address v)
  | JLE (`Address v)
  | JMP (`Address v)
  | CALL (`Address (_, v)) ->
    Address.collect ~tbl v
  | RET -> ()

let pp_unresolved_target ppf = function
  | `Literal (v, Some flag) -> pf ppf "%s@%s" v flag
  | `Literal (v, None) -> pf ppf "%s" v
  | `Address v -> pf ppf "%a" Address.pp_unresolved v

let pp_unresolved ppf = function
  | MOVZ (size, sr, a, b) ->
    pf ppf "movz%a%a\t%a, %a"
      Size.pp size
      (pp_option Size.pp) sr
      Address.pp_unresolved a
      Address.pp_unresolved b
  | MOVABS (size, a, b) ->
    pf ppf "movabs%a\t%a, %a"
      (pp_option Size.pp) size
      Address.pp_unresolved a
      Address.pp_unresolved b

  | SET (flag, x) ->
    pf ppf "set%c\t%a" flag Address.pp_unresolved x
  | INC (size, x) ->
    pf ppf "inc%a\t%a"
      (pp_option Size.pp) size
      Address.pp_unresolved x

  | JE target -> pf ppf "je\t%a" pp_unresolved_target target
  | JG target -> pf ppf "jg\t%a" pp_unresolved_target target
  | JB target -> pf ppf "jb\t%a" pp_unresolved_target target
  | JL target -> pf ppf "jl\t%a" pp_unresolved_target target
  | JNE target -> pf ppf "jne\t%a" pp_unresolved_target target
  | JGE target -> pf ppf "jge\t%a" pp_unresolved_target target
  | JLE target -> pf ppf "jle\t%a" pp_unresolved_target target
  | JMP target -> pf ppf "jmp\t%a" pp_unresolved_target target

  | CALL (`Literal (v, Some flag)) ->
    pf ppf "call\t%s@%s" v flag
  | CALL (`Literal (v, None)) ->
    pf ppf "call\t%s" v
  | CALL (`Address (true, v)) ->
    pf ppf "call\t*%a" Address.pp_unresolved v
  | CALL (`Address (false, v)) ->
    pf ppf "call\t%a" Address.pp_unresolved v

  | RET -> pf ppf "ret"

  | MOV (size, a, b) ->
    pf ppf "mov%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | SAR (size, a, b) ->
    pf ppf "sar%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | LEA (size, a, b) ->
    pf ppf "lea%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | ADD (size, a, b) ->
    pf ppf "add%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | AND (size, a, b) ->
    pf ppf "and%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | XOR (size, a, b) ->
    pf ppf "xor%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | CMP (size, a, b) ->
    pf ppf "cmp%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | SHR (size, a, b) ->
    pf ppf "shr%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | SUB (size, a, b) ->
    pf ppf "sub%a\t%a, %a" (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b
  | OR  (size, a, b) ->
    pf ppf "or%a\t%a, %a"  (pp_option Size.pp) size Address.pp_unresolved a Address.pp_unresolved b

let mov  ?size a b = MOV (Option.map Size.of_char size, a, b)
let sar  ?size a b = SAR (Option.map Size.of_char size, a, b)
let lea  ?size a b = LEA (Option.map Size.of_char size, a, b)
let _or  ?size a b = OR  (Option.map Size.of_char size, a, b)
let _and ?size a b = AND (Option.map Size.of_char size, a, b)
let add  ?size a b = ADD (Option.map Size.of_char size, a, b)
let cmp  ?size a b = CMP (Option.map Size.of_char size, a, b)
let xor  ?size a b = XOR (Option.map Size.of_char size, a, b)
let shr  ?size a b = SHR (Option.map Size.of_char size, a, b)
let sub  ?size a b = SUB (Option.map Size.of_char size, a, b)
let inc  ?size x   = INC (Option.map Size.of_char size, x)

let set ~flag v = SET (flag, v)

let jb v  = JB v
let je v  = JE v
let jg v  = JG v
let jl v  = JL v
let jne v = JNE v
let jge v = JGE v
let jle v = JLE v
let jmp v = JMP v

let ret = RET
let call v = CALL v

let movabs ?size a b =
  MOVABS (Option.map Size.of_char size, a, b)

let movz ?size:size_register size a b =
  MOVZ (Size.of_char size, Option.map Size.of_char size_register, a, b)

type g =
  | MOVZ   of Size.t * Address.t * Address.t
  | MOVABS of Address.t * Address.t
  | MOV    of Address.t * Address.t
  | SAR    of Address.t * Address.t
  | LEA    of Address.t * Address.t
  | ADD    of Address.t * Address.t
  | AND    of Address.t * Address.t
  | XOR    of Address.t * Address.t
  | CMP    of Address.t * Address.t
  | SHR    of Address.t * Address.t
  | SUB    of Address.t * Address.t
  | OR     of Address.t * Address.t

  | SET    of [ `E | `Z ] * Address.t

  | INC    of Address.t

  | J      of condition * Address.t
  | JMP    of Address.t

  | CALL   of Address.t
  | RET

and condition =
  [ `E | `B | `G | `L | `NE | `GE | `LE ]

let pp_binary_op
  : Format.formatter -> instr:string -> Address.t -> Address.t -> unit
  = fun ppf ~instr a b -> match Size.equal (Address.size a) (Address.size b) with
    | true  ->
      pf ppf "%s\t%a, %a" instr Address.pp a Address.pp b
    | false ->
      let size = Address.size a in
      pf ppf "%s%a\t%a, %a" instr Size.pp size Address.pp a Address.pp b

let pp_condition ppf = function
  | `E -> pf ppf "e"
  | `B -> pf ppf "b"
  | `G -> pf ppf "g"
  | `L -> pf ppf "l"
  | `NE -> pf ppf "ne"
  | `GE -> pf ppf "ge"
  | `LE -> pf ppf "le"

let pp
  : Format.formatter -> g -> unit
  = fun ppf -> function
    | RET -> pf ppf "ret"
    | J (c, v) -> pf ppf "j%a\t%a" pp_condition c Address.pp v
    | JMP v -> pf ppf "jmp\t%a" Address.pp v
    | CALL v -> pf ppf "call\t%a" Address.pp v
    | MOVZ (size, a, b) ->
      pf ppf "movz%a%a\t%a, %a" Size.pp size Size.pp (Address.size b) Address.pp a Address.pp b
    | MOVABS (a, b) -> pp_binary_op ppf ~instr:"movabs" a b
    | MOV (a, b) -> pp_binary_op ppf ~instr:"mov" a b
    | SAR (a, b) -> pp_binary_op ppf ~instr:"sar" a b
    | LEA (a, b) -> pp_binary_op ppf ~instr:"lea" a b
    | ADD (a, b) -> pp_binary_op ppf ~instr:"add" a b
    | AND (a, b) -> pp_binary_op ppf ~instr:"and" a b
    | XOR (a, b) -> pp_binary_op ppf ~instr:"xor" a b
    | CMP (a, b) -> pp_binary_op ppf ~instr:"cmp" a b
    | SHR (a, b) -> pp_binary_op ppf ~instr:"shr" a b
    | SUB (a, b) -> pp_binary_op ppf ~instr:"sub" a b
    | OR  (a, b) -> pp_binary_op ppf ~instr:"or"  a b
    | SET (`E, x) -> pf ppf "sete\t%a" Address.pp x
    | SET (`Z, x) -> pf ppf "setz\t%a" Address.pp x
    | INC x -> pf ppf "inc\t%a" Address.pp x

let ctor_of_binary_instr
  : unresolved -> Address.t -> Address.t -> g
  = fun instr a b -> match instr with
    | MOV _ -> MOV (a, b)
    | SAR _ -> SAR (a, b)
    | LEA _ -> LEA (a, b)
    | ADD _ -> ADD (a, b)
    | AND _ -> AND (a, b)
    | XOR _ -> XOR (a, b)
    | CMP _ -> CMP (a, b)
    | SHR _ -> SHR (a, b)
    | SUB _ -> SUB (a, b)
    | OR  _ -> OR  (a, b)
    | MOVABS _ -> MOVABS (a, b)
    | v -> invalid_arg "%a is not a binary instruction" pp_unresolved_instr v

let ctor_of_unary_instr
  : unresolved -> Address.t -> g
  = fun instr v -> match instr with
    | JE _ -> J (`E, v)
    | JG _ -> J (`G, v)
    | JL _ -> J (`L, v)
    | JB _ -> J (`B, v)
    | JNE _ -> J (`NE, v)
    | JGE _ -> J (`GE, v)
    | JLE _ -> J (`LE, v)
    | JMP _ -> JMP v
    | CALL _ -> CALL v
    | v -> invalid_arg "%a is not an unary instruction" pp_unresolved_instr v

let resolve ~tbl : unresolved -> g = fun instr -> match instr with
  | RET -> RET
  | JE (`Literal (name,   flag))
  | JG (`Literal (name,   flag))
  | JL (`Literal (name,   flag))
  | JB (`Literal (name,   flag))
  | JNE (`Literal (name,  flag))
  | JGE (`Literal (name,  flag))
  | JLE (`Literal (name,  flag))
  | JMP (`Literal (name,  flag))
  | CALL (`Literal (name, flag)) ->
    let literal = Option.fold ~none:name ~some:(( ^ ) name) flag in
    let v = Address.immediate ~size:Size.Quad ~literal (Hashtbl.find tbl name) in
    ctor_of_unary_instr instr v
  | JE (`Address v)
  | JG (`Address v)
  | JL (`Address v)
  | JB (`Address v)
  | JNE (`Address v)
  | JGE (`Address v)
  | JLE (`Address v)
  | JMP (`Address v)
  | CALL (`Address (_, v)) ->
    let v = Address.v ~tbl ~size:Size.Quad v in
    ( match Size.equal Size.Quad (Address.size v) with
      | true  -> ctor_of_unary_instr instr v
      | false -> invalid_arg "%a expects a quad address" pp_unresolved_instr instr )
  | SET (flag, v) ->
    let flag = match flag with
      | 'e' -> `E
      | 'z' -> `Z
      | _   -> assert false in
    let v = Address.v ~tbl v in
    SET (flag, v)
  | MOV (size, a, b)
  | SAR (size, a, b)
  | LEA (size, a, b)
  | ADD (size, a, b)
  | AND (size, a, b)
  | XOR (size, a, b)
  | CMP (size, a, b)
  | SHR (size, a, b)
  | SUB (size, a, b)
  | OR  (size, a, b)
  | MOVABS (size, a, b) ->
    let va = Address.v ~tbl ?size a in
    let vb = Address.v ~tbl ?size b in
    let sa = Address.size va in
    let sb = Address.size vb in
    ( match size, Size.(sa >= sb) with
      | _, true       -> ctor_of_binary_instr instr va vb
      | Some _, false -> assert false
      | None, false   ->
        invalid_arg "Impossible to %a from %a to %a" pp_unresolved_instr instr Address.pp va Address.pp vb )
  | MOVZ (size, size_register, a, b) ->
    let a = Address.v ~tbl ~size a in
    let b = Address.v ~tbl ?size:size_register b in
    let size_a = Address.size a in
    let size_b = Address.size b in
    ( match Size.(size <= size_b), Size.(size_a <= size_b) with
      | true, true -> MOVZ (size, a, b)
      | _ ->
        invalid_arg "Impossible to %a from %a to %a" pp_unresolved_instr instr Address.pp a Address.pp b )
  | INC (size, v) ->
    let v = Address.v ~tbl ?size v in
    INC v
