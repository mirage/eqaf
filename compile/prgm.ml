let ( <.> ) f g = fun x -> f (g x)
let error_msg err = Error (`Msg err)
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

type unresolved =
  (*
  | ALIGN of (string * (string * string option) option) option
  | QUAD of string
  | DATA of string option
  | TEXT of string option
  | GLOBAL of string
  | SECTION of string * (string * string option) option
  | TYPE of string * string
  | FILE of string
  | SIZE of string * Expr.unresolved
  | ASCII of string
  | SPACE of string * string option
  | BYTE of Expr.unresolved list
  | WORD of Expr.unresolved list
  | LONG of Expr.unresolved list
  *)

  (*
  | CFI_STARTPROC
  | CFI_ENDPROC
  | CFI_ADJUST_CFA_OFFSET of string
  *)

  | LINE of string
  | INSTR of Instr.unresolved
  | LABEL of string

let collect ~needed ~defined =
  let n = ref 0 in
  let go = function
    | LINE _ -> ()
    | INSTR instr -> Instr.collect ~tbl:needed instr ; incr n
    | LABEL v -> Hashtbl.add defined v !n ; incr n in
  List.iter go

let index ?(off = 0) ~predicate arr =
  if off >= Array.length arr then invalid_arg "index" ;
  let rec go i =
    if i >= Array.length arr then error_msg "Any elements don't respect the given predicate"
    else match predicate arr.(i) with
    | `Continue -> go (succ i)
    | `Stop -> Ok i in
  go off

let extract_one symbol prgm =
  let to_break = function
    | INSTR Instr.RET -> `Stop
    | INSTR (Instr.JMP _) -> `Stop
    | _ -> `Continue in
  let to_symbol = function
    | LABEL v ->
      if v = symbol
      then `Stop else `Continue
    | _ -> `Continue in
  let open Rresult in
  index ~off:0 ~predicate:to_symbol prgm >>= fun isymbol ->
  index ~off:isymbol ~predicate:to_break prgm >>= fun iret ->
  let func = Array.sub prgm isymbol (iret - isymbol + 1) in
  Ok (isymbol, func)

let diff a (b : (string, 'b) Hashtbl.t) =
  let module Set = Set.Make(String) in
  let sa = Hashtbl.fold (fun k _ s -> Set.add k s) a Set.empty in
  let sb = Hashtbl.fold (fun k _ s -> Set.add k s) b Set.empty in
  let r = Set.diff sa sb in
  let tbl = Hashtbl.create 0x10 in
  Set.iter (fun k -> Hashtbl.add tbl k ()) r ; tbl

let rec extract ~visited symbol prgm =
  let open Rresult in
  extract_one symbol prgm >>= fun (abs, func) ->
  let needed = Hashtbl.create 0x10 in
  let defined = Hashtbl.create 0x10 in
  collect ~needed ~defined (Array.to_list func) ;
  let undefined = diff needed defined in
  try
    let idx = Hashtbl.find defined symbol in
    Hashtbl.add visited symbol (abs,func) ; 
    assert (idx = 0) ;
    match Hashtbl.fold (fun k () a -> k :: a) undefined [] with
    | [] -> Ok ()
    | symbols ->
      let iter symbol =
        if not (Hashtbl.mem visited symbol)
        then match extract ~visited symbol prgm with
          | Ok () -> ()
          | Error (`Msg err) -> invalid_arg err in
      List.iter iter symbols ; Ok ()
  with Not_found -> error_msgf "%s is not properly defined" symbol
     | Invalid_argument err -> error_msg err

let extract symbol prgm =
  let open Rresult in
  let visited = Hashtbl.create 0x10 in
  let prgm = Array.of_list prgm in
  extract ~visited symbol prgm >>| fun () ->
  let res = Hashtbl.fold (fun k (idx, func) a -> (k, (idx, func)) :: a) visited [] in
  let compare (_, (a, _)) (_, (b, _)) = Int.compare a b in
  List.sort compare res

let pf = Format.fprintf

let pp_comma ppf () = pf ppf ","

let pp_list ~sep:pp_sep pp_val ppf =
  let rec go = function
    | [] -> ()
    | [ x ] -> pp_val ppf x
    | x :: r -> pf ppf "%a%a" pp_val x pp_sep () ; go r in
  go

let pp_unresolved ppf = function
  | INSTR v -> pf ppf "\t%a\n" Instr.pp_unresolved v
  | LABEL v -> pf ppf "%s:\n" v
  | LINE v -> pf ppf "\t%s\n" v

type g =
  | Instr of Instr.g
  | Label of string

let pp ppf = function
  | Instr v -> pf ppf "\t%a\n" Instr.pp v
  | Label v -> pf ppf "%s:\n" v

let resolve prgm =
  let defined = Hashtbl.create 0x10 in
  let cur = ref 0 in
  let collect : unresolved -> unit = function
    | LINE _ -> ()
    | INSTR _ -> incr cur
    | LABEL v -> Hashtbl.add defined v (Int64.of_int !cur) ; incr cur in
  List.iter collect prgm ;
  let resolve : g list -> unresolved -> g list = fun a -> function
    | LINE _ -> a
    | INSTR instr -> Instr (Instr.resolve ~tbl:defined instr) :: a
    | LABEL v -> Label v :: a in
  (List.rev <.> List.fold_left resolve []) prgm
