type eq = |
type lt = |
type gt = |

type ('a, 'b) t =
  | Lt : ('a, 'b) t
  | Eq : ('a, 'a) t
  | Gt : ('a, 'b) t

type ('a, 'b, 'c) lower_or_equal =
  | Lt : ('a, 'b, lt) lower_or_equal
  | Eq : ('a, 'a, eq) lower_or_equal

type ('a, 'b) lteq = LtEq : ('a, 'b, 'c) lower_or_equal -> ('a, 'b) lteq

let to_lower_or_equal : type a b. (a, b) t -> (a, b) lteq option = function
  | Lt -> Some (LtEq Lt)
  | Eq -> Some (LtEq Eq)
  | Gt -> None

