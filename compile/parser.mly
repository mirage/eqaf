%{
  let pf = Format.fprintf
  let strf = Format.asprintf

  let pp_comma ppf () = pf ppf ", "
  let pp_string ppf = pf ppf "%s"

  let pp_list ~sep:pp_sep pp_val ppf =
    let rec go = function
      | [] -> ()
      | [ x ] -> pp_val ppf x
      | x :: r -> pf ppf "%a%a" pp_val x pp_sep () ; go r in
    go
%}

%token EOL
%token EOF
%token <string> DIGIT
%token <string> LITERAL
%token <string> STRING

%token ASCII
%token SPACE
%token LONG
%token WORD
%token BYTE
%token SIZE
%token FILE
%token DATA
%token QUAD
%token TEXT
%token TYPE
%token ALIGN
%token GLOBAL
%token SECTION

%token CFI_ENDPROC
%token CFI_STARTPROC
%token CFI_ADJUST_CFA_OFFSET

%token CALL
%token <char option> MOVABS
%token <char option> INC
%token <char option> XOR
%token <char option> SHR
%token <char option> SUB
%token <char option> MOV
%token <char option> SAR
%token <char option> ADD
%token <char option> AND
%token <char option> CMP
%token <char option> OR
%token <char * char option> MOVZ
%token <char option> LEA
%token <char> SET
%token RET
%token JNE
%token JMP
%token JGE
%token JLE
%token JB
%token JG
%token JE
%token JL

%token LPAREN
%token RPAREN
%token PERCENT
%token DOLLAR
%token COMMA
%token DOT
%token COLON
%token DASH
%token SLASH
%token PLUS
%token STAR
%token AT

%start <Prgm.unresolved list> main

%%

let number == ~ = DIGIT; <>
let register == PERCENT; ~ = LITERAL; <>
let immediate == DOLLAR; ~ = DIGIT; <>

let base  == ~ = register; <>
let index == COMMA; ~ = register; <>
let scale == COMMA; ~ = number; <>

(* segment:?disp?(base,index,scale) *)
(* segment:?disp?(base,index,) *)
(* segment:?disp?(base,index) *)
(* segment:?disp?(,index,scale) *)
(* segment:?disp?(index,scale) *)
(* segment:?disp?(,index,) *)
(* segment:?disp?(,index) *)
(* segment:?disp?(index,) *)
(* segment:?disp?(index) *)

let disp ==
  | v = LITERAL; AT; flag = LITERAL; { `Literal (v, Some ("@" ^ flag)) }
  | v = LITERAL; PERCENT; flag = LITERAL; { `Literal (v, Some ("%" ^ flag)) }
  | v = LITERAL; { `Literal (v, None) }
  | v = number; { `Number v }

let segment_and_disp ==
  | segment = LITERAL; COLON; disp = disp; { Some segment, Some disp }
  | disp = disp; { None, Some disp }
  | { None, None }

let indirect ==
  | front = segment_and_disp;
            LPAREN; base = base; RPAREN;
            { let segment, disp = front in Address.unresolved_indirect ?segment ?disp ~base () }
  | front = segment_and_disp;
    LPAREN; base = base; index = index; RPAREN;
    { let segment, disp = front in Address.unresolved_indirect ?segment ?disp ~base ~index () }
  | front = segment_and_disp;
    LPAREN; base = base; index = index; scale = scale; RPAREN;
    { let segment, disp = front in Address.unresolved_indirect ?segment ?disp ~base ~index ~scale () }
  | front = segment_and_disp;
    LPAREN; index = index; scale = scale; RPAREN;
    { let segment, disp = front in Address.unresolved_indirect ?segment ?disp ~index ~scale () }
  | front = segment_and_disp;
    LPAREN; scale = scale; RPAREN;
    { let segment, disp = front in Address.unresolved_indirect ?segment ?disp ~scale () }

let address :=
  | v = register;  { Address.unresolved_register v }
  | v = immediate; { Address.unresolved_immediate v }
  | ~ = indirect; <>

let align ==
  | ALIGN; pad = DIGIT; COMMA; value = DIGIT; COMMA; max = DIGIT;
    { strf "\t.align\t%s, %s, %s" pad value max }
  | ALIGN; pad = DIGIT; COMMA; value = DIGIT;
    { strf "\t.align\t%s, %s" pad value }
  | ALIGN; pad = DIGIT;
    { strf "\t.align\t%s" pad }

let data ==
  | DATA; v = DIGIT;        { strf "\t.data %s" v }
  | DATA;                   { strf "\t.data" }
let text ==
  | TEXT; v = DIGIT;        { strf "\t.text %s" v }
  | TEXT;                   { strf "\t.text" }
let global ==
  | GLOBAL; v = LITERAL;    { strf "\t.globl\t%s" v }
let file ==
  | FILE; v = STRING;       { strf "\t.file %S" v }

let section_type ==
  | AT; v = LITERAL; { strf "@%s" v }
  | PERCENT; v = LITERAL; { strf "%%%s" v }
let section_flags ==
  | v = STRING; COMMA; t = section_type; { strf "%S,%s" v t }
  | v = STRING; { v }
let section ==
  | SECTION; name = LITERAL; COMMA; flags = section_flags;
    { strf "\t.section %s,%s" name flags }
  | SECTION; name = LITERAL;
    { strf "\t.section %s" name }

let ty ==
  | TYPE; name = LITERAL; COMMA; AT; t = LITERAL;
    { strf "\t.type %s,@%s" name t }
  | TYPE; name = LITERAL; COMMA; PERCENT; t = LITERAL;
    { strf "\t.type %s,%%%s" name t }

let label ==
  | v = LITERAL; COLON; { Prgm.LABEL v }

let app(f, x) == ~ = f; ~ = x; { f x }

let fold_left(op, elem) :=
  | elem
  | sum = fold_left(op, elem); ~ = op; ~ = elem; { op sum elem }

let unary_operator ==
  | DASH;  { strf "- %s" }
let mul_or_div ==
  | STAR;  { strf "%s * %s" }
  | SLASH; { strf "%s / %s" }
let add_or_sub ==
  | PLUS;  { strf "%s + %s" }
  | DASH;  { strf "%s - %s" }

let add_or_sub_expr ==
  fold_left(add_or_sub, mul_or_div_expr)
let mul_or_div_expr ==
  fold_left(mul_or_div, atomic_expr)

let atomic_expr :=
  | DOT; { "." }
  | v = DIGIT; { v }
  | v = LITERAL; { v }
  | LPAREN; expr = expr; RPAREN; { strf "(%s)" expr }
  | app(unary_operator, atomic_expr)

let expr == add_or_sub_expr

let size :=
  | SIZE; name = LITERAL; COMMA; expr = expr; { strf "\t.size %s,%s" name expr }

let address_or_literal ==
  | v = address; { `Address v }
  | v = LITERAL; AT; flag = LITERAL; { `Literal (v, Some ("@" ^ flag)) }
  | v = LITERAL; PERCENT; flag = LITERAL; { `Literal (v, Some ("%" ^ flag)) }
  | v = LITERAL; { `Literal (v, None) }

let instr :=
  | modifier = MOVZ; a = address; COMMA; b = address;   { let v, size = modifier in Instr.movz ?size v a b }
  | size = MOVABS; a = address; COMMA; b = address; { Instr.movabs ?size a b }

  | size = MOV; a = address; COMMA; b = address;    { Instr.mov ?size a b }
  | size = SAR; a = address; COMMA; b = address;    { Instr.sar ?size a b }
  | size = LEA; a = address; COMMA; b = address;    { Instr.lea ?size a b }
  | size = ADD; a = address; COMMA; b = address;    { Instr.add ?size a b }
  | size = CMP; a = address; COMMA; b = address;    { Instr.cmp ?size a b }
  | size = XOR; a = address; COMMA; b = address;    { Instr.xor ?size a b }
  | size = AND; a = address; COMMA; b = address;    { Instr._and ?size a b }
  | size = SUB; a = address; COMMA; b = address;    { Instr.sub ?size a b }
  | size = SHR; a = address; COMMA; b = address;    { Instr.shr ?size a b }
  | size = OR; a = address; COMMA; b = address;     { Instr._or ?size a b }

  | size = INC; x = address;                        { Instr.inc ?size x }

  | flag = SET; x = address;                        { Instr.set ~flag x }

  | RET; { Instr.ret }
  | JG; v = address_or_literal;                     { Instr.jg v }
  | JE; v = address_or_literal;                     { Instr.je v }
  | JB; v = address_or_literal;                     { Instr.jb v }
  | JL; v = address_or_literal;                     { Instr.jl v }
  | JNE; v = address_or_literal;                    { Instr.jne v }
  | JGE; v = address_or_literal;                    { Instr.jge v }
  | JLE; v = address_or_literal;                    { Instr.jle v }
  | JMP; v = address_or_literal;                    { Instr.jmp v }

  | CALL; STAR; v = address;                        { Instr.call (`Address (true, v)) }
  | CALL; v = LITERAL;                              { Instr.call (`Literal (v, None)) }
  | CALL; v = LITERAL; AT; flag = LITERAL;          { Instr.call (`Literal (v, Some ("@" ^ flag))) }
  | CALL; v = address;                              { Instr.call (`Address (false, v)) }

let space ==
  | SPACE; size = DIGIT; COMMA; fill = DIGIT; { strf "\t.space\t%s, %s" size fill }
  | SPACE; size = DIGIT;                      { strf "\t.space\t%s" size }

let byte == BYTE; lst = separated_list(COMMA, expr); { strf "\t.byte\t%a" (pp_list ~sep:pp_comma pp_string) lst }
let word == WORD; lst = separated_list(COMMA, expr); { strf "\t.word\t%a" (pp_list ~sep:pp_comma pp_string) lst }
let long == LONG; lst = separated_list(COMMA, expr); { strf "\t.long\t%a" (pp_list ~sep:pp_comma pp_string) lst }
let quad == QUAD; lst = separated_list(COMMA, expr); { strf "\t.quad\t%a" (pp_list ~sep:pp_comma pp_string) lst }

let prgm :=
  | ~ = align;   <Prgm.LINE>
  | ~ = quad;    <Prgm.LINE>
  | ~ = text;    <Prgm.LINE>
  | ~ = data;    <Prgm.LINE>
  | ~ = global;  <Prgm.LINE>
  | ~ = section; <Prgm.LINE>
  | ~ = ty;      <Prgm.LINE>
  | ~ = file;    <Prgm.LINE>
  | ~ = size;    <Prgm.LINE>
  | ~ = space;   <Prgm.LINE>
  | ~ = byte;    <Prgm.LINE>
  | ~ = word;    <Prgm.LINE>
  | ~ = long;    <Prgm.LINE>
  | ASCII; v = STRING;                 { Prgm.LINE (strf "\t.ascii\t%S" v) }
  | CFI_STARTPROC;                     { Prgm.LINE "\t.cfi_startproc" }
  | CFI_ENDPROC;                       { Prgm.LINE "\t.cfi_endproc" }
  | CFI_ADJUST_CFA_OFFSET; v = number; { Prgm.LINE (strf "\t.cfi_adjust_cfa_offset %s" v) }
  (* XXX(dinosaure): we are interested only by that. *)
  | instr = instr;     { Prgm.INSTR instr }
  | label = label; <>

let line := ~ = prgm; EOL+; <>

let main := ~ = line*; EOF; <>

%%
(*

let pf = Format.fprintf

let pp_option pp_val ppf = function
  | Some x -> pp_val ppf x
  | None -> ()

let pp_char ppf v = pf ppf "%c" v

let pp_token ppf = function
  | EOL -> pf ppf "EOL"
  | EOF -> pf ppf "EOF"
  | DIGIT v -> pf ppf "(DIGIT %S)" v
  | LITERAL v -> pf ppf "(LITERAL %S)" v
  | STRING v -> pf ppf "(STRING %S)" v
  | FILE -> pf ppf "FILE"
  | SECTION -> pf ppf "SECTION"
  | COMMA -> pf ppf "COMMA"
  | AT -> pf ppf "AT"
  | PLUS -> pf ppf "PLUS"
  | SLASH -> pf ppf "SLASH"
  | STAR -> pf ppf "STAR"
  | ALIGN -> pf ppf "ALIGN"
  | COLON -> pf ppf "COLON"
  | QUAD -> pf ppf "QUAD"
  | DASH -> pf ppf "DASH"
  | DATA -> pf ppf "DATA"
  | TEXT -> pf ppf "TEXT"
  | GLOBAL -> pf ppf "GLOBAL"
  | MOVABS v -> pf ppf "(MOVABS %a)" (pp_option pp_char) v
  | CFI_STARTPROC -> pf ppf "CFI_STARTPROC"
  | CFI_ENDPROC -> pf ppf "CFI_ENDPROC"
  | CFI_ADJUST_CFA_OFFSET -> pf ppf "CFI_ADJUST_CFA_OFFSET"
  | BYTE -> pf ppf "BYTE"
  | WORD -> pf ppf "WORD"
  | LONG -> pf ppf "LONG"
  | PERCENT -> pf ppf "PERCENT"
  | SPACE -> pf ppf "SPACE"
  | ASCII -> pf ppf "ASCII"
  | SAR v -> pf ppf "(SAR %a)" (pp_option pp_char) v
  | MOV v -> pf ppf "(MOV %a)" (pp_option pp_char) v
  | MOVZ (a, b) -> pf ppf "(MOVZB (%a, %a))" (pp_option pp_char) a (pp_option pp_char) b
  | LEA v -> pf ppf "(LEA %a)" (pp_option pp_char) v
  | CALL -> pf ppf "CALL"
  | CMP v -> pf ppf "(CMP %a)" (pp_option pp_char) v
  | AND v -> pf ppf "(AND %a)" (pp_option pp_char) v
  | XOR v -> pf ppf "(XOR %a)" (pp_option pp_char) v
  | SHR v -> pf ppf "(SHR %a)" (pp_option pp_char) v
  | SUB v -> pf ppf "(SUB %a)" (pp_option pp_char) v
  | SET v -> pf ppf "(SET %c)" v
  | RET -> pf ppf "RET"
  | JNE -> pf ppf "JNE"
  | JGE -> pf ppf "JGE"
  | JLE -> pf ppf "JLE"
  | JMP -> pf ppf "JMP"
  | INC v -> pf ppf "(INC %a)" (pp_option pp_char) v
  | JG -> pf ppf "JG"
  | JE -> pf ppf "JE"
  | JL -> pf ppf "JL"
  | ADD v -> pf ppf "(ADD %a)" (pp_option pp_char) v
  | OR v -> pf ppf "(OR %a)" (pp_option pp_char) v
  | LPAREN -> pf ppf "LPAREN"
  | RPAREN -> pf ppf "RPAREN"
  | TYPE -> pf ppf "TYPE"
  | SIZE -> pf ppf "SIZE"
  | DOT -> pf ppf "DOT"
  | DOLLAR -> pf ppf "DOLLAR"
*)
