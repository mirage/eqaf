{
  open Lexing

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
      | None -> pos.pos_fname
      | Some file -> file in
    lexbuf.lex_curr_p <-
      { pos with pos_fname= new_file
               ; pos_lnum = if absolute then line else pos.pos_lnum + line
               ; pos_bol= pos.pos_cnum - chars }

  let invalid_arg fmt = Format.kasprintf invalid_arg fmt

  let digit = function
    | 'a' .. 'f' as chr -> 10 + Char.code chr - Char.code 'a'
    | 'A' .. 'F' as chr -> 10 + Char.code chr - Char.code 'A'
    | '0' .. '9' as chr -> Char.code chr - Char.code '0'
    | _ -> assert false

  let to_int ~base ~first ~last lexbuf =
    let code = ref 0 in
    for i = first to last do
      let v = digit (Lexing.lexeme_char lexbuf i) in
      assert (v < base) ;
      code := (base * !code) + v ;
    done ; !code

  let char_for_decimal_code lexbuf i =
    let code = to_int lexbuf ~base:10 ~first:i ~last:(i+2) in
    if code > 255 || code < 0 then invalid_arg "Invalid code of character \\%03d" code ;
    Char.chr code

  let char_for_octal_code lexbuf i =
    let code = to_int lexbuf ~base:8 ~first:i ~last:(i+2) in
    if code > 255 || code < 0 then invalid_arg "Invalid code of character \\%03o" code ;
    Char.chr code

  let char_for_hexadecimal_code lexbuf i =
    let code = to_int lexbuf ~base:16 ~first:i ~last:(i+1) in
    Char.chr code

  open Parser
}

let dec_literal = [ '0'-'9' ] [ '0'-'9' ]*
let hex_literal = '0' [ 'x' 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]*
let oct_literal = '0' [ 'o' 'O' ] [ '0'-'7' ] [ '0'-'7' ]*
let bin_literal = '0' [ 'b' 'B' ] [ '0'-'1' ] [ '0'-'1' ]*
let int_literal = dec_literal | hex_literal | oct_literal | bin_literal

let hex_digit = [ '0'-'9' 'A'-'F' 'a'-'f' ]

let newline = '\r'* '\n'
let size = [ 'b' 'w' 'l' 'q' ]
let wsp = [ ' ' '\t' ]
let literal = [ 'a'-'z' 'A'-'Z' '_' '0'-'9' '.' '-' ]
let number = '-'? int_literal

rule parser = parse
  | newline { update_loc lexbuf None 1 false 0 ; EOL }
  | wsp+ { parser lexbuf }
  | number as v { DIGIT v }

  (* directive *)
  | ".section" { SECTION }
  | ".global"  { GLOBAL }
  | ".align"   { ALIGN }
  | ".globl"   { GLOBAL }
  | ".text"    { TEXT }
  | ".quad"    { QUAD }
  | ".data"    { DATA }
  | ".type"    { TYPE }
  | ".file"    { FILE }
  | ".size"    { SIZE }
  | ".ascii"   { ASCII }
  | ".space"   { SPACE }
  | ".byte"    { BYTE }
  | ".word"    { WORD }
  | ".long"    { LONG }

  | ".cfi_startproc" { CFI_STARTPROC }
  | ".cfi_endproc"   { CFI_ENDPROC }
  | ".cfi_adjust_cfa_offset" { CFI_ADJUST_CFA_OFFSET }

  (* instr *)
  | "mov" (size as size)   { MOV (Some size) }
  | "mov"                  { MOV None }
  | "sar" (size as size)   { SAR (Some size) }
  | "sar"                  { SAR None }
  | "or" (size as size)    { OR (Some size) }
  | "or"                   { OR None }
  | "and" (size as size)   { AND (Some size) }
  | "and"                  { AND None }
  | "add" (size as size)   { ADD (Some size) }
  | "add"                  { ADD None }
  | "cmp" (size as size)   { CMP (Some size) }
  | "cmp"                  { CMP None }
  | "lea" (size as size)   { LEA (Some size) }
  | "lea"                  { LEA None }
  | "xor" (size as size)   { XOR (Some size) }
  | "xor"                  { XOR None }
  | "shr" (size as size)   { SHR (Some size) }
  | "shr"                  { SHR None }
  | "sub" (size as size)   { SUB (Some size) }
  | "sub"                  { SUB None }

  | "set" ([ 'e' 'z' ] as flag) { SET flag }
  | "inc" (size as size)        { INC (Some size) }
  | "inc"                       { INC None }

  | "movz" (size as a) (size as b) { MOVZ (a, Some b) }
  | "movz" (size as a)             { MOVZ (a, None) }

  | "movabs" (size as size)        { MOVABS (Some size) }
  | "movabs"                       { MOVABS None }

  | "call"                 { CALL }

  | "ret"                  { RET }
  | "jne"                  { JNE }
  | "jmp"                  { JMP }
  | "jge"                  { JGE }
  | "jle"                  { JLE }
  | "jg"                   { JG }
  | "jb"                   { JB }
  | "je"                   { JE }
  | "jl"                   { JL }

  (* quoted-string *)
  | '"' { let buf = Buffer.create 16 in string buf lexbuf }

  | "/*" { comment lexbuf }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '.' { DOT }
  | ':' { COLON }
  | '-' { DASH }
  | '*' { STAR }
  | '/' { SLASH }
  | '+' { PLUS }
  | '@' { AT }
  | '%' { PERCENT }
  | '$' { DOLLAR }

  | literal+ as v { LITERAL v }

  | eof { EOF }
and string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' ([ '\\' '\'' '"' 'n' 't' 'b' 'r' ' ' ] as chr)
    { let chr = match chr with
          | 'n' -> '\n'
          | 't' -> '\t'
          | 'b' -> '\b'
          | 'r' -> '\r'
          | chr -> chr in
      Buffer.add_char buf chr ; string buf lexbuf }
  | '\\' [ '0'-'9' ]
    { let code = Char.code (Lexing.lexeme_char lexbuf 1) - Char.code '0' in
      let chr = Char.chr code in
      Buffer.add_char buf chr ; string buf lexbuf }
  | '\\' [ '0'-'9' ] [ '0'-'9' ] [ '0'-'9' ]
    { let chr = char_for_decimal_code lexbuf 1 in
      Buffer.add_char buf chr ; string buf lexbuf }
  | '\\' 'o' [ '0'-'7' ] [ '0'-'7' ] [ '0'-'7' ]
    { let chr = char_for_octal_code lexbuf 2 in
      Buffer.add_char buf chr ; string buf lexbuf }
  | '\\' 'x' [ 'a'-'f' 'A'-'F' '0'-'9' ] [ 'a'-'f' 'A'-'F' '0'-'9' ]
    { let chr = char_for_hexadecimal_code lexbuf 2 in
      Buffer.add_char buf chr ; string buf lexbuf }
  | eof { invalid_arg "Unterminated quoted-string" }
  | _ as chr
    { Buffer.add_char buf chr ; string buf lexbuf }
and comment = parse
  | "*/" { parser lexbuf }
  | _ { comment lexbuf }

{

}
