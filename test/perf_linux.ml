type kind =
  [ `Cycles
  | `Instructions
  | `Cache_references
  | `Cache_misses
  | `Branch_instructions
  | `Branch_misses
  | `Bus_cycles
  | `Stalled_cycles_frontend
  | `Stalled_cycles_backend
  | `Ref_cpu_cycles
  | `Cpu_clock
  | `Task_clock
  | `Page_faults
  | `Context_switches
  | `Cpu_migrations
  | `Page_faults_min
  | `Page_faults_maj
  | `Alignement_faults
  | `Emulation_faults
  | `Dummy ]

let kind_to_string : kind -> string = function
  | `Cycles -> "cycles"
  | `Instructions -> "instructions"
  | `Cache_references -> "cache-references"
  | `Cache_misses -> "caches-misses"
  | `Branch_instructions -> "branch-instructions"
  | `Branch_misses -> "branch-misses"
  | `Bus_cycles -> "bus-cycles"
  | `Stalled_cycles_frontend -> "stalled-cycles-frontend"
  | `Stalled_cycles_backend -> "stalled-cyles-backend"
  | `Ref_cpu_cycles -> "ref-cpu-cycles"
  | `Cpu_clock -> "cpu-clock"
  | `Task_clock -> "task-clock"
  | `Page_faults -> "page-faults"
  | `Context_switches -> "context-switches"
  | `Cpu_migrations -> "cpu-migrations"
  | `Page_faults_min -> "page-faults-min"
  | `Page_faults_maj -> "page-faults-maj"
  | `Alignement_faults -> "alignement-faults"
  | `Emulation_faults -> "emulation-faults"
  | `Dummy -> "dummy"

let pp_kind ppf kind = Fmt.string ppf (kind_to_string kind)

let kind_to_int : kind -> int = function
  | `Cycles -> 0
  | `Instructions -> 1
  | `Cache_references -> 2
  | `Cache_misses -> 3
  | `Branch_instructions -> 4
  | `Branch_misses -> 5
  | `Bus_cycles -> 6
  | `Stalled_cycles_frontend -> 7
  | `Stalled_cycles_backend -> 8
  | `Ref_cpu_cycles -> 9
  | `Cpu_clock -> 10
  | `Task_clock -> 11
  | `Page_faults -> 12
  | `Context_switches -> 13
  | `Cpu_migrations -> 14
  | `Page_faults_min -> 15
  | `Page_faults_maj -> 16
  | `Alignement_faults -> 17
  | `Emulation_faults -> 18
  | `Dummy -> 19

let string_to_kind : string -> kind = function
  | "cycles" -> `Cycles
  | "instructions" -> `Instructions
  | "cache-references" -> `Cache_references
  | "cache-misses" -> `Cache_misses
  | "branch-instructions" -> `Branch_instructions
  | "branch-misses" -> `Branch_misses
  | "bus-cycles" -> `Bus_cycles
  | "stalled-cycles-frontend" -> `Stalled_cycles_frontend
  | "stalled-cycles-backend" -> `Stalled_cycles_backend
  | "ref-cpu-cycles" -> `Ref_cpu_cycles
  | "cpu-clock" -> `Cpu_clock
  | "task-clock" -> `Task_clock
  | "page-faults" -> `Page_faults
  | "context-switches" -> `Context_switches
  | "cpu-migrations" -> `Cpu_migrations
  | "page-faults-min" -> `Page_faults_min
  | "page-faults-maj" -> `Page_faults_maj
  | "alignement-faults" -> `Alignement_faults
  | "emulation-faults" -> `Emulation_faults
  | "dummy" -> `Dummy
  | x -> Fmt.invalid_arg "Perf.string_to_kind: %s" x

let int_to_kind : int -> kind = function
  | 0 -> `Cycles
  | 1 -> `Instructions
  | 2 -> `Cache_references
  | 3 -> `Cache_misses
  | 4 -> `Branch_instructions
  | 5 -> `Branch_misses
  | 6 -> `Bus_cycles
  | 7 -> `Stalled_cycles_frontend
  | 8 -> `Stalled_cycles_backend
  | 9 -> `Ref_cpu_cycles
  | 10 -> `Cpu_clock
  | 11 -> `Task_clock
  | 12 -> `Page_faults
  | 13 -> `Context_switches
  | 14 -> `Cpu_migrations
  | 15 -> `Page_faults_min
  | 16 -> `Page_faults_maj
  | 17 -> `Alignement_faults
  | 18 -> `Emulation_faults
  | 19 -> `Dummy
  | x -> Fmt.invalid_arg "Perf.int_to_kind: %d" x

let compare_kind a b = kind_to_int a - kind_to_int b

module Attr = struct
  type flag =
    [ `Disabled
    | `Inherit
    | `Exclude_user
    | `Exclude_kernel
    | `Exclude_hv
    | `Exclude_idle
    | `Enable_on_exec ]

  let flag_to_int = function
    | `Disabled -> 0
    | `Inherit -> 1
    | `Exclude_user -> 2
    | `Exclude_kernel -> 3
    | `Exclude_hv -> 4
    | `Exclude_idle -> 5
    | `Enable_on_exec -> 6

  let flag_to_id = function
    | `Disabled -> 0b0000001
    | `Inherit -> 0b0000010
    | `Exclude_user -> 0b0000100
    | `Exclude_kernel -> 0b0001000
    | `Exclude_hv -> 0b0010000
    | `Exclude_idle -> 0b0100000
    | `Enable_on_exec -> 0b1000000

  let compare_flag a b = flag_to_int a - flag_to_int b

  module FlagSet = Set.Make (struct
    type t = flag

    let compare = compare_flag
  end)

  type t = {flags: FlagSet.t; kind: kind}

  let make ?(flags = []) kind = {flags= FlagSet.of_list flags; kind}
end

type flag = [`Fd_cloexec | `Fd_no_group | `Fd_output | `Pid_cgroup]

let flag_to_int = function
  | `Fd_cloexec -> 0
  | `Fd_no_group -> 1
  | `Fd_output -> 2
  | `Pid_cgroup -> 3

let flag_to_id = function
  | `Fd_cloexec -> 0b0001
  | `Fd_no_group -> 0b0010
  | `Fd_output -> 0b0100
  | `Pid_cgroup -> 0b1000

let compare_flag a b = flag_to_int a - flag_to_int b

module FlagSet = Set.Make (struct
  type t = flag

  let compare = compare_flag
end)

type t = {fd: Unix.file_descr; kind: kind}

external perf_event_open :
     kind:int
  -> attr_flags:int
  -> pid:int
  -> cpu:int
  -> group_fd:int
  -> flags:int
  -> Unix.file_descr
  = "perf_linux_event_open_bytes" "perf_linux_event_open"

external perf_event_ioc_enable :
  Unix.file_descr -> unit
  = "perf_linux_event_enable"

external perf_event_ioc_disable :
  Unix.file_descr -> unit
  = "perf_linux_event_disable"

let make ?(pid = 0) ?(cpu = -1) ?group ?(flags = []) attr =
  let flags = FlagSet.of_list flags in
  let flags = FlagSet.fold (fun f acc -> acc + flag_to_id f) flags 0 in
  let attr_flags =
    let open Attr in
    FlagSet.fold (fun f acc -> acc + flag_to_id f) attr.flags 0
  in
  let group_fd =
    match group with Some {fd; _} -> (Obj.magic fd : int) | None -> -1
  in
  let kind = kind_to_int attr.kind in
  { fd= perf_event_open ~kind ~attr_flags ~pid ~cpu ~group_fd ~flags
  ; kind= attr.kind }

let enable {fd; _} = perf_event_ioc_enable fd

let disable {fd; _} = perf_event_ioc_disable fd

let read {fd; _} =
  let buf = Bytes.create 8 in
  let rd = Unix.read fd buf 0 8 in
  assert (rd = 8) ;
  if Sys.big_endian then assert false else assert false

module Set = Set.Make (struct
  type t = kind

  let compare = compare_kind
end)

module Map = Map.Make (struct
  type t = kind

  let compare = compare_kind
end)
