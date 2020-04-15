let exit_success = 0
let pf = Format.fprintf
let pr = Format.printf

module Asm = Asm
open Rresult

let ( <.> ) f g = fun x -> f (g x)

module Interpreter = Parser.MenhirInterpreter

let pp_position ppf { Lexing.pos_lnum; Lexing.pos_cnum; Lexing.pos_bol; _ } =
  Format.fprintf ppf "l.%d c.%d" pos_lnum (pos_cnum - pos_bol)

let compile_interface filename =
  let open Bos in
  let cmd = Cmd.(v "ocamlopt" % "-c" % (Fpath.to_string filename) % "-o" % Fpath.(to_string <.> base) filename) in
  OS.Cmd.run cmd >>| fun () -> Fpath.set_ext "cmi" filename

let generate_assembly_file ~mli filename =
  let open Rresult in
  let open Bos in
  let cmd = Cmd.(v "ocamlopt" % "-intf" % (Fpath.to_string mli) % "-S" % "-c" % (Fpath.to_string filename)) in
  OS.Cmd.run cmd >>| fun () -> Fpath.set_ext "s" filename

let parse_assembly filename =
  Bos.OS.File.with_ic filename @@ fun ic () ->
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  let lexer () =
    let token = Lexer.parser lexbuf in
    (token, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
  let rec go (checkpoint : Prgm.unresolved list Interpreter.checkpoint) =
    match checkpoint with
    | Interpreter.InputNeeded _env ->
      let token, start, stop = lexer () in
      let checkpoint = Interpreter.offer checkpoint (token, start, stop) in
      go checkpoint
    | Interpreter.Shifting _ | Interpreter.AboutToReduce _ ->
      let checkpoint = Interpreter.resume checkpoint in
      go checkpoint
    | Interpreter.HandlingError env ->
      let start, stop = Interpreter.positions env in
      R.reword_error (R.msgf "Error on %a at %a" Fpath.pp filename Fmt.(pair pp_position pp_position)) (Error (start, stop))
    | Interpreter.Accepted t -> Ok t
    | Interpreter.Rejected -> assert false in
  go ((Parser.Incremental.main <.> Lexing.lexeme_start_p) lexbuf)

let output_assembly filename prgm =
  Bos.OS.File.with_oc filename @@ fun oc () ->
  let ppf = Format.formatter_of_out_channel oc in
  List.iter (Prgm.pp_unresolved ppf) prgm ; Ok ()

external bool_of_int : int -> bool = "%identity"

let camlEqaf__equal_155 prgm v0 v1 =
  let heap = Heap.make ~len:(Heap.needed_string v0 + Heap.needed_string v1) in
  let rax_value = Heap.inject_string heap v0 in
  let rbx_value = Heap.inject_string heap v1 in
  let registers = Registers.make () in
  registers.Register.%[Register.rax] <- Int64.of_int rax_value ;
  registers.Register.%[Register.rbx] <- Int64.of_int rbx_value ;
  Asm.execute "camlEqaf__equal_155" ~registers ~heap prgm >>| fun ticks ->
  bool_of_int (Int64.to_int registers.Register.%[Register.rax] lsr 1), ticks

let ticks_when_lengths_mistmatch = 19
let expected_ticks x = x * 15 + 42

let fuzz_camlEqaf__equal_155 prgm =
  Crowbar.add_test ~name:"camlEqaf__equal_155" Crowbar.[ bytes; bytes; ] @@ fun v0 v1 ->
  let expect_res = String.equal v0 v1 in
  let expect_ticks = if String.length v0 <> String.length v1
    then ticks_when_lengths_mistmatch
    else expected_ticks (String.length v0) in
  match camlEqaf__equal_155 prgm v0 v1 with
  | Ok (res, ticks) ->
    Fmt.epr "> res:%b, expect_res: %b.\n%!" res expect_res ;
    Crowbar.check_eq res expect_res ;
    Fmt.epr "> ticks:%d, expect_ticks: %d.\n%!" ticks expect_ticks
  | Error (`Msg err) -> Crowbar.fail err

let () =
  let ml = Fpath.v "lib/eqaf.ml" in
  let mli = Fpath.v "lib/eqaf.mli" in
  let res =
    compile_interface mli >>= fun _intf ->
    generate_assembly_file ~mli ml >>= fun s ->
    parse_assembly s () |> R.join >>= fun prgm ->
    output_assembly (Fpath.set_ext "ss" s) prgm () |> R.join >>= fun _ ->
    fuzz_camlEqaf__equal_155 prgm ; Ok () in
  R.failwith_error_msg res
