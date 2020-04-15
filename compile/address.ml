let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let strf fmt = Format.asprintf fmt
let ( <.> ) f g = fun x -> f (g x)
let identity x = x

type unresolved_disp =
  [ `Number of string
  | `Literal of string * string option ]

type unresolved_indirect =
  { segment : string option
  ; disp : unresolved_disp option
  ; base : string option
  ; index : string option
  ; scale : string option }

type unresolved =
  | Register of string
  | Indirect of unresolved_indirect
  | Immediate of string

let unresolved_indirect ?segment ?disp ?base ?scale ?index () : unresolved =
  Indirect { segment; disp; base; scale; index; }

let unresolved_immediate v = Immediate v
let unresolved_register v = Register v

let pf = Format.fprintf

let pp_unresolved_segment ppf = function
  | None -> ()
  | Some v -> pf ppf "%s:" v

let pp_unresolved_disp ppf = function
  | None -> ()
  | Some (`Number v) -> pf ppf "%s" v
  | Some (`Literal (v, Some flag)) -> pf ppf "%s%s" v flag
  | Some (`Literal (v, None)) -> pf ppf "%s" v

let pp_unresolved_base ppf = function
  | None -> ()
  | Some base -> pf ppf "%%%s" base

let pp_unresolved_index ppf = function
  | None -> ()
  | Some index -> pf ppf ",%%%s" index

let pp_unresolved_scale ppf = function
  | None -> ()
  | Some scale -> pf ppf ",%s" scale

let pp_unresolved : Format.formatter -> unresolved -> unit = fun ppf -> function
  | Register v -> pf ppf "%%%s" v
  | Immediate v -> pf ppf "$%s" v
  | Indirect { segment
             ; disp
             ; base
             ; index
             ; scale } ->
    pf ppf "%a%a(%a%a%a)"
      pp_unresolved_segment segment
      pp_unresolved_disp disp
      pp_unresolved_base base
      pp_unresolved_index index
      pp_unresolved_scale scale

let collect ~tbl = function
  | Register _ -> ()
  | Immediate _ -> ()
  | Indirect { segment; disp; _ } -> match segment, disp with
    | Some segment, Some (`Literal (name, _)) ->
      Hashtbl.replace tbl segment () ;
      Hashtbl.replace tbl name ()
    | Some segment, _ ->
      Hashtbl.replace tbl segment ()
    | None, Some (`Literal (name, _)) ->
      Hashtbl.replace tbl name ()
    | _ -> ()

type process =
     ?base:int64
  -> ?index:int64
  -> unit -> int64
type t =
  | Register of { size : Size.t
                ; register : Register.t }
  | Indirect of { pp : string
                ; size : Size.t
                ; base : Register.t option
                ; index : Register.t option
                ; process : process }
  | Immediate of { literal : string option
                 ; size : Size.t
                 ; value : int64 }

let pp : Format.formatter -> t -> unit = fun ppf -> function
  | Register { register; _ } -> Register.pp ppf register
  | Immediate { literal= None; size; value; } -> pf ppf ("$" ^^ (Size.hexadecimal size)) value
  | Immediate { literal= Some literal; size; value; } ->
    pf ppf ("%s<" ^^ (Size.decimal size) ^^ ">") literal value
  | Indirect { pp; _ } -> pf ppf "%s" pp

let size : t -> Size.t = function
  | Register  { size; _ } -> size
  | Indirect  { size; _ } -> size
  | Immediate { size; _ } -> size

let register : Size.t -> Register.t -> t = fun size register ->
  let size' = Register.size register in
  match Size.(size <= size') with
  | true  -> Register { size; register; }
  | false -> invalid_arg "Impossible to cast value of %a to %a" Register.pp register Size.pp size

let immediate
  : size:Size.t -> ?literal:string -> int64 -> t
  = fun ~size ?literal value ->
    let ( land ) = Int64.logand in
    match size with
    | Size.Byte -> Immediate { literal; size; value= (value land 0xffL) }
    | Size.Word -> Immediate { literal; size; value= (value land 0xffffL) }
    | Size.Long -> Immediate { literal; size; value= (value land 0xffffffffL) }
    | Size.Quad -> Immediate { literal; size; value; }

let indirect ~tbl ~pp ~size { segment= _; disp; base= v0; index= v1; scale; } : t =
  let r0 = Option.map Register.v v0 in
  let r1 = Option.map Register.v v1 in
  let s0 = Option.fold ~none:Size.Quad ~some:Register.size r0 in
  let s1 = Option.fold ~none:Size.Quad ~some:Register.size r1 in
  let mask size value = match size with
    | Size.Byte -> Int64.logand value 0xffL
    | Size.Word -> Int64.logand value 0xffffL
    | Size.Long -> Int64.logand value 0xffffffffL
    | Size.Quad -> value in
  let disp = match disp with
    | None -> 0L
    | Some (`Number v) -> Int64.of_string v
    | Some (`Literal (name, _)) -> Hashtbl.find tbl name in
  let scale = Option.(value (map Int64.of_string scale) ~default:1L) in
  let process ?base:v0 ?index:v1 () =
    let v0 = Option.fold ~none:0L ~some:(mask s0) v0 in
    let v1 = Option.fold ~none:0L ~some:(mask s1) v1 in
    let ( + ) = Int64.add and ( * ) = Int64.mul in
    v0 + (v1 * scale) + disp in
  Indirect { pp; size; base= r0; index= r1; process; }

let v
  : tbl:(string, int64) Hashtbl.t -> ?size:Size.t -> unresolved -> t
  = fun ~tbl ?size unresolved ->
    (* let t = resolve ~tbl unresolved in *)
    match size, unresolved with
  | Some size, Immediate v -> immediate ~size (Int64.of_string v)
  | None, Register reg ->
    let reg = Register.v reg in
    let size = Register.size reg in
    register size reg
  | Some size, Register reg ->
    let reg = Register.v reg in
    register size reg
  | Some size, Indirect v ->
    let pp = strf "%a" pp_unresolved unresolved in
    indirect ~tbl ~pp ~size v 
  | None, Immediate v -> immediate ~size:Size.Word (Int64.of_string v) 
  | None, Indirect v ->
    let pp = strf "%a" pp_unresolved unresolved in
    indirect ~tbl ~pp ~size:Size.Word v 
