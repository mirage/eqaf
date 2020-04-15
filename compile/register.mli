type t

val is_register : string -> bool

val pp : Format.formatter -> t -> unit
val v : string -> t
val size : t -> Size.t
val eip : t
val rsi : t
val rdi : t
val rax : t
val rbx : t

val (.%[]) : int64 array -> t -> int64
val (.%[]<-) : int64 array -> t -> int64 -> unit

val (.![]) : int64 array -> (Size.t * t) -> int64
val (.![]<-) : int64 array -> (Size.t * t) -> int64 -> unit
