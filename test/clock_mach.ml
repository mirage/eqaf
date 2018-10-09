type kind =
  [ `Realtime
  | `Monotonic
  | `Realtime_coarse
  | `Monotonic_coarse
  | `Monotonic_raw
  | `Boot_time
  | `Process_cpu_time
  | `Thread_cpu_time ]

external clock_mach_get_time : unit -> int64 = "clock_mach_get_time"

let get = function
  | `Realtime | `Realtime_coarse | `Monotonic_coarse | `Monotonic_raw
   |`Boot_time | `Process_cpu_time | `Thread_cpu_time ->
      Fmt.invalid_arg "clock not available on Mach"
  | `Monotonic -> clock_mach_get_time ()

let kind_to_string = function
  | `Realtime -> "realtime"
  | `Monotonic -> "monotonic"
  | `Realtime_coarse -> "realtime-coarse"
  | `Monotonic_coarse -> "monotonic-coarse"
  | `Monotonic_raw -> "monotonic-raw"
  | `Boot_time -> "boot-time"
  | `Process_cpu_time -> "process-cpu-time"
  | `Thread_cpu_time -> "thread-cpu-time"

let pp_kind ppf kind = Fmt.string ppf (kind_to_string kind)

let kind_to_int = function
  | `Realtime -> 0
  | `Monotonic -> 1
  | `Realtime_coarse -> 2
  | `Monotonic_coarse -> 3
  | `Monotonic_raw -> 4
  | `Boot_time -> 5
  | `Process_cpu_time -> 6
  | `Thread_cpu_time -> 7

let string_to_kind = function
  | "realtime" -> `Realtime
  | "monotonic" -> `Monotonic
  | "realtime-coarse" -> `Realtime_coarse
  | "monotonic-coarse" -> `Monotonic_coarse
  | "monotonic-raw" -> `Monotonic_raw
  | "boot-time" -> `Boot_time
  | "process-cpu-time" -> `Process_cpu_time
  | "thread-cpu-time" -> `Thread_cpu_time
  | x -> Fmt.invalid_arg "Clock_mach.string_to_kind: %s" x

let int_to_kind = function
  | 0 -> `Realtime
  | 1 -> `Monotonic
  | 2 -> `Realtime_coarse
  | 3 -> `Monotonic_coarse
  | 4 -> `Monotonic_raw
  | 5 -> `Boot_time
  | 6 -> `Process_cpu_time
  | 7 -> `Thread_cpu_time
  | x -> Fmt.invalid_arg "Clock_mach.int_to_kind: %d" x

let max = 8

let compare_kind a b = kind_to_int a - kind_to_int b

module Set = Set.Make (struct
  type t = kind

  let compare = compare_kind
end)

module Map = Map.Make (struct
  type t = kind

  let compare = compare_kind
end)
