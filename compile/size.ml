let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let ( <.> ) f g = fun x -> f (g x)

type quad = int64
type long = int32
type word = int
type byte = char

type t =
  | Byte
  | Word
  | Long
  | Quad

let pf = Format.fprintf

let pp ppf = function
  | Byte -> pf ppf "b"
  | Word -> pf ppf "w"
  | Long -> pf ppf "l"
  | Quad -> pf ppf "q"

let of_char = function
  | 'b' -> Byte
  | 'w' -> Word
  | 'l' -> Long
  | 'q' -> Quad
  | chr -> invalid_arg "Invalid size: %c" chr

let equal a b = match a, b with
  | Byte, Byte -> true
  | Word, Word -> true
  | Long, Long -> true
  | Quad, Quad -> true
  | _ -> false

let hexadecimal : t -> (int64 -> 'a, 'b, 'c, 'a) format4 = function
  | Byte -> "%02Lx"
  | Word -> "%04Lx"
  | Long -> "%08Lx"
  | Quad -> "%016Lx"

let decimal : t -> (int64 -> 'a, 'b, 'c, 'a) format4 = function
  | Byte -> "%02Ld"
  | Word -> "%04Ld"
  | Long -> "%08Ld"
  | Quad -> "%016Ld"

let mask = function
  | Byte -> 0xffL
  | Word -> 0xffffL
  | Long -> 0xffffffffL
  | Quad -> 0xffffffffffffffffL

let compare a b =
  let inf = (-1) and sup = 1 in
  match a, b with
  | Byte, Byte -> 0
  | Byte, _ -> inf
  | Word, Word -> 0
  | Word, Byte -> sup
  | Word, _ -> inf
  | Long, Long -> 0
  | Long, (Byte | Word) -> sup
  | Long, _ -> inf
  | Quad, Quad -> 0
  | Quad, (Byte | Word | Long) -> sup

let ( < ) a b = compare a b < 0
let ( > ) a b = compare a b > 0
let ( <= ) a b = compare a b <= 0
let ( >= ) a b = compare a b >= 0
