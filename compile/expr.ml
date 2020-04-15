type unresolved =
  | DOT
  | DIGIT of string
  | LITERAL of string
  | ADD of unresolved * unresolved
  | SUB of unresolved * unresolved
  | MUL of unresolved * unresolved
  | DIV of unresolved * unresolved
  | NEG of unresolved

let dot = DOT
let digit v = DIGIT v
let literal v = LITERAL v
let add a b = ADD (a, b)
let sub a b = SUB (a, b)
let mul a b = MUL (a, b)
let div a b = DIV (a, b)
let neg x = NEG x

let pf = Format.fprintf

let rec pp ppf = function
  | DOT -> pf ppf "."
  | DIGIT v -> pf ppf "%s" v
  | LITERAL v -> pf ppf "%s" v
  | ADD (a, b) -> pf ppf "%a + %a" pp a pp b
  | SUB (a, b) -> pf ppf "%a - %a" pp a pp b
  | DIV (a, b) -> pf ppf "%a * %a" pp a pp b
  | MUL (a, b) -> pf ppf "%a / %a" pp a pp b
  | NEG v -> pf ppf "- %a" pp v

let rec iter ~f = function
  | ADD (x, y)
  | SUB (x, y)
  | MUL (x, y)
  | DIV (x, y) ->
    iter ~f x ;
    iter ~f y
  | NEG x ->
    iter ~f x ;
  | DOT -> f `DOT
  | LITERAL v -> f (`LITERAL v)
  | DIGIT v -> f (`DIGIT v)

let unresolved expr =
  let dot = ref false in
  let tbl = Hashtbl.create 0x10 in
  let f = function
    | `DOT -> dot := true || !dot
    | `LITERAL v -> Hashtbl.replace tbl v ()
    | `DIGIT _ -> () in
  iter ~f expr ;
  if !dot
  then `DOT :: Hashtbl.fold (fun v () a -> `LITERAL v :: a) tbl []
  else Hashtbl.fold (fun v () a -> `LITERAL v :: a) tbl []

let resolve ~dot ~tbl expr =
  let exception Not_found of string in
  let rec go = function
    | ADD (x, y) ->
      let x = go x and y = go y in
      Int64.add x y
    | SUB (x, y) ->
      let x = go x and y = go y in
      Int64.sub x y
    | MUL (x, y) ->
      let x = go x and y = go y in
      Int64.mul x y
    | DIV (x, y) ->
      let x = go x and y = go y in
      Int64.div x y
    | NEG x ->
      let x = go x in
      Int64.neg x
    | LITERAL v ->
      ( try Hashtbl.find tbl v
        with _exn -> raise (Not_found v) )
    | DIGIT v -> Int64.of_string v
    | DOT -> dot in
  try Ok (go expr)
  with Not_found literal -> Error (`Not_found literal)
