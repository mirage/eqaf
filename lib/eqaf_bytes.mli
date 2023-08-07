val equal : bytes -> bytes -> bool
val compare_be : bytes -> bytes -> int
val compare_be_with_len : len:int -> bytes -> bytes -> int
val compare_le : bytes -> bytes -> int
val compare_le_with_len : len:int -> bytes -> bytes -> int
val find_uint8 : ?off:int -> f:(int -> bool) -> bytes -> int
val exists_uint8 : ?off:int -> f:(int -> bool) -> bytes -> bool
