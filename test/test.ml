let () = Printexc.record_backtrace true

module type CLOCK = sig
  type kind =
    [ `Realtime
    | `Monotonic
    | `Realtime_coarse
    | `Monotonic_coarse
    | `Monotonic_raw
    | `Boot_time
    | `Process_cpu_time
    | `Thread_cpu_time ]

  val get : kind -> int64

  val kind_to_string : kind -> string

  val kind_to_int : kind -> int

  val string_to_kind : string -> kind

  val int_to_kind : int -> kind

  val max : int

  val pp_kind : kind Fmt.t

  module Set : Set.S with type elt = kind

  module Map : Map.S with type key = kind
end

module type PERF = sig
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

  val pp_kind : kind Fmt.t

  module Attr : sig
    type t

    type flag =
      [ `Disabled
      | `Inherit
      | `Exclude_user
      | `Exclude_kernel
      | `Exclude_hv
      | `Exclude_idle
      | `Enable_on_exec ]

    val make : ?flags:flag list -> kind -> t
  end

  type t

  type flag = [`Fd_cloexec | `Fd_no_group | `Fd_output | `Pid_cgroup]

  val make :
    ?pid:int -> ?cpu:int -> ?group:t -> ?flags:flag list -> Attr.t -> t

  val enable : t -> unit

  val disable : t -> unit

  val read : t -> int64

  val kind_to_string : kind -> string

  val kind_to_int : kind -> int

  val string_to_kind : string -> kind

  val int_to_kind : int -> kind

  module Set : Set.S with type elt = kind

  module Map : Map.S with type key = kind
end

module Field (Clock : CLOCK) (Perf : PERF) = struct
  type t =
    [ `Run
    | `Time of Clock.kind
    | `Perf of Perf.kind
    | `Minor_collection
    | `Major_collection
    | `Compaction
    | `Minor_allocated
    | `Major_allocated
    | `Promoted ]

  let to_int = function
    | `Run -> 0
    | `Minor_collection -> 1
    | `Major_collection -> 2
    | `Compaction -> 3
    | `Minor_allocated -> 4
    | `Major_allocated -> 5
    | `Promoted -> 6
    | `Time kind -> 7 + Clock.kind_to_int kind
    | `Perf kind -> 7 + Clock.max + Perf.kind_to_int kind

  let compare a b = to_int a - to_int b

  let pp ppf = function
    | `Run -> Fmt.string ppf "run"
    | `Time time -> Clock.pp_kind ppf time
    | `Perf perf -> Perf.pp_kind ppf perf
    | `Minor_collection -> Fmt.string ppf "minor-collection"
    | `Major_collection -> Fmt.string ppf "major-collection"
    | `Compaction -> Fmt.string ppf "compaction"
    | `Minor_allocated -> Fmt.string ppf "minor-allocated"
    | `Major_allocated -> Fmt.string ppf "major-allocated"
    | `Promoted -> Fmt.string ppf "promoted"

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module Measurement_raw (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)

  type t =
    { mutable run: int
    ; mutable time: int64 Clock.Map.t
    ; mutable perf: int64 Perf.Map.t
    ; mutable compaction: int
    ; mutable minor_allocated: float
    ; mutable major_allocated: float
    ; mutable promoted: float
    ; mutable major_collection: int
    ; mutable minor_collection: int }

  let pp ppf t =
    Fmt.pf ppf
      "{ @[<hov>run = %d;@ time = @[%a@];@ perf = @[%a@]; compaction = %d;@ \
       minor_allocated = %f;@ major_allocated = %f;@ promoted = %f;@ \
       major_collection = %d;@ minor_collection = %d;@] }"
      t.run
      Fmt.(Dump.list (Dump.pair Clock.pp_kind int64))
      (Clock.Map.bindings t.time)
      Fmt.(Dump.list (Dump.pair Perf.pp_kind int64))
      (Perf.Map.bindings t.perf) t.compaction t.minor_allocated
      t.major_allocated t.promoted t.major_collection t.minor_collection

  let run {run; _} = run

  let time kind {time; _} = Clock.Map.find kind time

  let perf kind {perf; _} = Perf.Map.find kind perf

  let compaction {compaction; _} = compaction

  let minor_allocated {minor_allocated; _} = minor_allocated

  let major_allocated {major_allocated; _} = major_allocated

  let promoted {promoted; _} = promoted

  let major_collection {major_collection; _} = major_collection

  let minor_collection {minor_collection; _} = minor_collection

  let ( <.> ) f g x = f (g x)

  let make () =
    { run= 0
    ; time= Clock.Map.empty
    ; perf= Perf.Map.empty
    ; compaction= 0
    ; minor_allocated= 0.
    ; major_allocated= 0.
    ; promoted= 0.
    ; minor_collection= 0
    ; major_collection= 0 }

  let accessor = function
    | `Run -> float_of_int <.> run
    | `Time kind -> Int64.to_float <.> time kind
    | `Perf kind -> Int64.to_float <.> perf kind
    | `Compaction -> float_of_int <.> compaction
    | `Minor_allocated -> minor_allocated
    | `Major_allocated -> major_allocated
    | `Promoted -> promoted
    | `Minor_collection -> float_of_int <.> minor_collection
    | `Major_collection -> float_of_int <.> major_collection
end

module Linear_algebra = struct
  let col_norm a column =
    let acc = ref 0. in
    for i = 0 to Array.length a - 1 do
      let entry = a.(i).(column) in
      acc := !acc +. (entry *. entry)
    done ;
    sqrt !acc

  let col_inner_prod t j1 j2 =
    let acc = ref 0. in
    for i = 0 to Array.length t - 1 do
      acc := !acc +. (t.(i).(j1) *. t.(i).(j2))
    done ;
    !acc

  let qr_in_place a =
    let m = Array.length a in
    if m = 0 then ([||], [||])
    else
      let n = Array.length a.(0) in
      let r = Array.make_matrix n n 0. in
      for j = 0 to n - 1 do
        let alpha = col_norm a j in
        r.(j).(j) <- alpha ;
        let one_over_alpha = 1. /. alpha in
        for i = 0 to m - 1 do
          a.(i).(j) <- a.(i).(j) *. one_over_alpha
        done ;
        for j2 = j + 1 to n - 1 do
          let c = col_inner_prod a j j2 in
          r.(j).(j2) <- c ;
          for i = 0 to m - 1 do
            a.(i).(j2) <- a.(i).(j2) -. (c *. a.(i).(j))
          done
        done
      done ;
      (a, r)

  let qr ?(in_place = false) a =
    let a = if in_place then a else Array.map Array.copy a in
    qr_in_place a

  let mul_mv ?(trans = false) a x =
    let rows = Array.length a in
    if rows = 0 then [||]
    else
      let cols = Array.length a.(0) in
      let m, n, get =
        if trans then
          let get i j = a.(j).(i) in
          (cols, rows, get)
        else
          let get i j = a.(i).(j) in
          (rows, cols, get)
      in
      if n <> Array.length x then failwith "Dimension mismatch" ;
      let result = Array.make m 0. in
      for i = 0 to m - 1 do
        let v, _ =
          Array.fold_left
            (fun (acc, j) x -> (acc +. (get i j *. x), succ j))
            (0., 0) x
        in
        result.(i) <- v
      done ;
      result

  let is_nan v = match classify_float v with FP_nan -> true | _ -> false

  let triu_solve r b =
    let m = Array.length b in
    if m <> Array.length r then
      invalid_arg
        "triu_solve R b requires R to be square with same number of rows as b"
    else if m = 0 then [||]
    else if m <> Array.length r.(0) then
      invalid_arg "triu_solve R b requires R to be square"
    else
      let sol = Array.copy b in
      for i = m - 1 downto 0 do
        sol.(i) <- sol.(i) /. r.(i).(i) ;
        for j = 0 to i - 1 do
          sol.(j) <- sol.(j) -. (r.(j).(i) *. sol.(i))
        done
      done ;
      if Array.exists is_nan sol then
        invalid_arg "triu_solve detected NaN result"
      else sol

  let ols ?(in_place = false) a b =
    let q, r = qr ~in_place a in
    triu_solve r (mul_mv ~trans:true q b)
end

module Config (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)

  type t = {responder: Field.t; predictors: Field.Set.t}

  let make ~predictors responder =
    {responder; predictors= Field.Set.of_list predictors}

  let run_vs predictors =
    {responder= `Run; predictors= Field.Set.of_list predictors}

  let run_vs_time =
    {responder= `Run; predictors= Field.Set.of_list [`Time `Monotonic]}
end

module Measures (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)
  module Config = Config (Clock) (Perf)

  type t = {clock: Clock.Set.t; perf: Perf.Set.t}

  let all =
    let all_clock =
      [ `Realtime
      ; `Monotonic
      ; `Realtime_coarse
      ; `Monotonic_coarse
      ; `Monotonic_raw
      ; `Boot_time
      ; `Process_cpu_time
      ; `Thread_cpu_time ]
    in
    let all_perf =
      [ `Cycles
      ; `Instructions
      ; `Cache_references
      ; `Cache_misses
      ; `Branch_instructions
      ; `Branch_misses
      ; `Bus_cycles
      ; `Stalled_cycles_frontend
      ; `Stalled_cycles_backend
      ; `Ref_cpu_cycles
      ; `Cpu_clock
      ; `Task_clock
      ; `Page_faults
      ; `Context_switches
      ; `Cpu_migrations
      ; `Page_faults_min
      ; `Page_faults_maj
      ; `Alignement_faults
      ; `Emulation_faults
      ; `Dummy ]
    in
    {clock= Clock.Set.of_list all_clock; perf= Perf.Set.of_list all_perf}

  let pp ppf {clock; perf} =
    Fmt.pf ppf "{ @[<hov>clock = @[%a@];@ perf = @[%a@];@] }"
      Fmt.(Dump.list Clock.pp_kind)
      (Clock.Set.elements clock)
      Fmt.(Dump.list Perf.pp_kind)
      (Perf.Set.elements perf)

  let of_config {Config.predictors; _} =
    List.fold_left
      (fun {clock; perf} -> function
        | `Time v -> {clock= Clock.Set.add v clock; perf}
        | `Perf v -> {perf= Perf.Set.add v perf; clock} | _ -> {clock; perf} )
      {clock= Clock.Set.empty; perf= Perf.Set.empty}
      (Field.Set.fold (fun x a -> x :: a) predictors [])

  let of_configs configs =
    List.fold_left
      (fun acc config ->
        let {clock; perf} = of_config config in
        { clock= Clock.Set.fold Clock.Set.add acc.clock clock
        ; perf= Perf.Set.fold Perf.Set.add acc.perf perf } )
      {clock= Clock.Set.empty; perf= Perf.Set.empty}
      configs
end

module Measurement (Clock : CLOCK) (Perf : PERF) = struct
  module Measurement_raw = Measurement_raw (Clock) (Perf)

  type t = {name: string; raw: Measurement_raw.t array}
end

module Coefficient (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)

  type t = {predictor: Field.t; estimate: float}

  let make ~predictor estimate = {predictor; estimate}
end

module Analyze (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)
  module Config = Config (Clock) (Perf)
  module Measurement_raw = Measurement_raw (Clock) (Perf)
  module Measurement = Measurement (Clock) (Perf)
  module Coefficient = Coefficient (Clock) (Perf)

  let make_matrix_and_vector ~responder ~predictors m =
    let predictors_acc = Array.map Measurement_raw.accessor predictors in
    let responder_acc = Measurement_raw.accessor responder in
    let make_predictor_values m =
      Array.map (fun predictor_acc -> predictor_acc m) predictors_acc
    in
    ( Array.init (Array.length m) (fun i -> make_predictor_values m.(i))
    , Array.init (Array.length m) (fun i -> responder_acc m.(i)) )

  let ols ~responder ~predictors m =
    let matrix, vector = make_matrix_and_vector ~responder ~predictors m in
    Linear_algebra.ols matrix vector

  type t = {name: string; analyzes: Coefficient.t array}

  let analyze config m =
    let responder = config.Config.responder in
    let predictors =
      Field.Set.elements config.Config.predictors |> Array.of_list
    in
    let coeffs = ols ~responder ~predictors m.Measurement.raw in
    let analyzes =
      Array.mapi
        (fun i predictor -> Coefficient.make ~predictor coeffs.(i))
        predictors
    in
    {name= m.Measurement.name; analyzes}
end

module Staged = struct
  type 'a t = 'a

  external stage : 'a -> 'a t = "%identity"

  external unstage : 'a t -> 'a = "%identity"
end

module Test (Clock : CLOCK) (Perf : PERF) = struct
  type packed = T : ([`Init] -> unit -> 'a) -> packed

  type basic = {name: string; fn: packed}

  type t = {name: string; basics: basic list}

  let make_indexed ~name ~args process =
    { name
    ; basics=
        List.map
          (fun v ->
            { name= Fmt.strf "%s:%d" name v
            ; fn= T (fun `Init -> Staged.unstage (process v)) } )
          args }
end

module Bench (Clock : CLOCK) (Perf : PERF) = struct
  module Field = Field (Clock) (Perf)
  module Config = Config (Clock) (Perf)
  module Measures = Measures (Clock) (Perf)
  module Measurement_raw = Measurement_raw (Clock) (Perf)
  module Measurement = Measurement (Clock) (Perf)
  module Test = Test (Clock) (Perf)
  module Analyze = Analyze (Clock) (Perf)

  let stabilize () =
    let rec loop fail last_heap_live_words =
      if fail <= 0 then
        failwith
          "unable to stabilize the number of live words in the major heap" ;
      Gc.compact () ;
      let stat = Gc.stat () in
      if stat.Gc.live_words <> last_heap_live_words then
        loop (fail - 1) stat.Gc.live_words
    in
    loop 10 0

  let exceeded_allowed_time quota time_0 =
    let time_1 = Clock.(get `Monotonic) in
    Int64.sub time_1 time_0 > quota

  let ( <*> ) a b = (a, b)

  let measure_one ~configs ~quota ~sampling test =
    let measures = Measures.of_configs configs in
    let (Test.T fn) = test.Test.fn in
    let fn = fn `Init in
    let time_init_0 = Clock.(get `Monotonic) in
    let results = Array.init 3000 (fun _ -> Measurement_raw.make ()) in
    let index = ref 0 in
    let run = ref 0 in
    let times_i =
      Clock.Set.fold (fun x a -> x :: a) measures.Measures.clock []
      |> List.map Clock.kind_to_int |> Array.of_list
    in
    while
      (not (exceeded_allowed_time quota time_init_0))
      && !index < Array.length results
    do
      let current_index = !index in
      let current_run = !run in
      if current_run = 0 then stabilize () ;
      let times_0 = Array.make (Array.length times_i) 0L in
      let times_1 = Array.make (Array.length times_i) 0L in
      let tags =
        List.map
          (fun kind -> Perf.(make (Attr.make kind)))
          (Perf.Set.fold (fun x a -> x :: a) measures.Measures.perf [])
        |> Array.of_list
      in
      (* GC, PERF, CLOCK *)
      let stat_0 = Gc.quick_stat () in
      Array.iter Perf.enable tags ;
      Array.iteri
        (fun i x -> times_0.(i) <- Clock.(get (int_to_kind x)))
        times_i ;
      (* BENCHMARK *)
      for _ = 1 to current_run do
        ignore (fn ())
      done ;
      (* BENCHMARK *)
      Array.iteri
        (fun i x -> times_1.(i) <- Clock.(get (int_to_kind x)))
        times_i ;
      Array.iter Perf.disable tags ;
      let stat_1 = Gc.quick_stat () in
      (* CLOCK, PERF, GC *)
      let _, times =
        Array.fold_left
          (fun (i, m) x ->
            (succ i, Clock.(Map.add (int_to_kind times_i.(i)) x m)) )
          (0, Clock.Map.empty)
          (Array.map2 Int64.sub times_1 times_0)
      in
      let perfs =
        Array.fold_left
          (fun m (k, t) -> Perf.Map.add k (Perf.read t) m)
          Perf.Map.empty
          (Array.map2 ( <*> )
             (Array.of_list
                (Perf.Set.fold (fun x a -> x :: a) measures.Measures.perf []))
             tags)
      in
      let result = results.(current_index) in
      result.Measurement_raw.run <- current_run ;
      result.Measurement_raw.time <- times ;
      result.Measurement_raw.perf <- perfs ;
      result.Measurement_raw.minor_allocated
      <- stat_1.Gc.minor_words -. stat_0.Gc.minor_words ;
      result.Measurement_raw.major_allocated
      <- stat_1.Gc.major_words -. stat_1.Gc.major_words ;
      result.Measurement_raw.promoted
      <- stat_1.Gc.promoted_words -. stat_0.Gc.promoted_words ;
      result.Measurement_raw.compaction
      <- stat_1.Gc.compactions - stat_0.Gc.compactions ;
      result.Measurement_raw.major_collection
      <- stat_1.Gc.major_collections - stat_0.Gc.major_collections ;
      result.Measurement_raw.minor_collection
      <- stat_1.Gc.minor_collections - stat_0.Gc.minor_collections ;
      incr index ;
      let next =
        match sampling with
        | `Linear k -> current_run + k
        | `Geometric scale ->
            let n = float_of_int current_run *. scale in
            max (int_of_float n) (current_run + 1)
      in
      run := next
    done ;
    let max = !index in
    {Measurement.name= test.Test.name; Measurement.raw= Array.sub results 0 max}

  let measure_all ~configs ~quota ~sampling basic_tests =
    List.map (measure_one ~configs ~quota ~sampling) basic_tests

  let measure_all ~configs ~quota ~sampling tests =
    let basic_tests =
      List.concat (List.map (fun test -> test.Test.basics) tests)
    in
    measure_all ~configs ~quota ~sampling basic_tests
end

let eqaf = Eqaf.equal

external unsafe_string_get : string -> int -> int = "%string_unsafe_get"
  [@@noalloc]

let eqml a b =
  let ln = String.length a in
  if ln = String.length b then (
    let rt = ref 0 in
    for i = 0 to ln - 1 do
      rt := !rt lor (unsafe_string_get a i lxor unsafe_string_get b i)
    done ;
    !rt = 0 )
  else false

external eqst : string -> string -> bool = "caml_string_equal"

let eqnt a b =
  let len = min (String.length a) (String.length b) in
  let res = ref 0n in
  let get_int i s = Nativeint.of_int (int_of_char (String.unsafe_get s i)) in
  for i = 0 to pred len do
    res := Nativeint.logor !res (Nativeint.logxor (get_int i a) (get_int i b))
  done ;
  String.length a = String.length b && !res = 0n

external unsafe_string_get_64 : string -> int -> int64 = "%caml_string_get64u"

let eqbg a b =
  let len = min (String.length a) (String.length b) in
  let res = ref 0L in
  for i = 0 to pred (len / 8) do
    res :=
      Int64.logor !res
        (Int64.logxor
           (unsafe_string_get_64 a (i * 8))
           (unsafe_string_get_64 a (i * 8)))
  done ;
  for i = len - (len mod 8) to pred len do
    res :=
      Int64.logor !res
        (Int64.logxor
           (Int64.of_int (unsafe_string_get a i))
           (Int64.of_int (unsafe_string_get b i)))
  done ;
  String.length a = String.length b && !res = 0L

let random_hash digest_size _ =
  let get _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.(chr (code '0' + n))
    | n when n < 10 + 26 -> Char.(chr (code 'a' + n - 10))
    | n -> Char.(chr (code 'A' + n - 10 - 26))
  in
  String.init digest_size get

module Make (Clock : CLOCK) (Perf : PERF) = struct
  module Config = Config (Clock) (Perf)
  module Test = Test (Clock) (Perf)
  module Analyze = Analyze (Clock) (Perf)
  module Bench = Bench (Clock) (Perf)
  module Coefficient = Coefficient (Clock) (Perf)

  let bench_eqaf hashes_0 hashes_1 n =
    Test.make_indexed ~name:"eqaf" ~args:(List.init n (fun x -> x))
    @@ fun i ->
    let a = hashes_0.(i) in
    let b = hashes_1.(i) in
    Staged.stage (fun () -> eqaf a b)

  let bench_eqst hashes_0 hashes_1 n =
    Test.make_indexed ~name:"eqst" ~args:(List.init n (fun x -> x))
    @@ fun i ->
    let a = hashes_0.(i) in
    let b = hashes_1.(i) in
    Staged.stage (fun () -> eqst a b)

  let bench_eqml hashes_0 hashes_1 n =
    Test.make_indexed ~name:"eqml" ~args:(List.init n (fun x -> x))
    @@ fun i ->
    let a = hashes_0.(i) in
    let b = hashes_1.(i) in
    Staged.stage (fun () -> eqml a b)

  let bench_eqnt hashes_0 hashes_1 n =
    Test.make_indexed ~name:"eqnt" ~args:(List.init n (fun x -> x))
    @@ fun i ->
    let a = hashes_0.(i) in
    let b = hashes_1.(i) in
    Staged.stage (fun () -> eqnt a b)

  let bench_eqbg hashes_0 hashes_1 n =
    Test.make_indexed ~name:"eqbg" ~args:(List.init n (fun x -> x))
    @@ fun i ->
    let a = hashes_0.(i) in
    let b = hashes_1.(i) in
    Staged.stage (fun () -> eqbg a b)

  let measurements ~configs hashes_0 hashes_1 n =
    Bench.measure_all ~configs ~quota:1000000000L ~sampling:(`Geometric 1.01)
      [ bench_eqaf hashes_0 hashes_1 n
      ; bench_eqst hashes_0 hashes_1 n
      ; bench_eqml hashes_0 hashes_1 n
      ; bench_eqnt hashes_0 hashes_1 n
      ; bench_eqbg hashes_0 hashes_1 n ]

  let measure_and_analyze ~configs hashes_0 hashes_1 n =
    List.map
      (fun m -> List.map (fun config -> Analyze.analyze config m) configs)
      (measurements ~configs hashes_0 hashes_1 n)

  type kind = Eqaf | Eqst | Eqml | Eqnt | Eqbg

  let kind_of_string name =
    match String.sub name 0 4 with
    | "eqaf" -> Eqaf
    | "eqst" -> Eqst
    | "eqml" -> Eqml
    | "eqnt" -> Eqnt
    | "eqbg" -> Eqbg
    | _ -> invalid_arg "kind_of_string"

  let is_eqaf = function Eqaf, _ -> true | _ -> false

  let is_eqml = function Eqml, _ -> true | _ -> false

  let is_eqst = function Eqst, _ -> true | _ -> false

  let is_eqnt = function Eqnt, _ -> true | _ -> false

  let is_eqbg = function Eqbg, _ -> true | _ -> false

  let times hashes_0 hashes_1 n =
    let configs = [Config.run_vs_time] in
    let get_run_vs_time = List.hd in
    List.map
      (fun a ->
        ( kind_of_string a.Analyze.name
        , a.Analyze.analyzes.(0).Coefficient.estimate ) )
      (List.map get_run_vs_time
         (measure_and_analyze ~configs hashes_0 hashes_1 n))
end

let digest_size = 4096

let tests = 50

module R (Clock : CLOCK) (Perf : PERF) = struct
  module Make = Make (Clock) (Perf)
  open Make

  let hashes_0, hashes_1 =
    let same0 = random_hash digest_size 0 in
    let same1 = Bytes.to_string (Bytes.of_string same0) in
    assert (same0 != same1) ;
    (* [eqst] short-cut this case. *)
    let where = Random.int tests in
    let hashes_0 =
      Array.init tests (fun i ->
          if i = where then same0 else random_hash digest_size i )
    in
    let hashes_1 =
      Array.init tests (fun i ->
          if i = where then same1 else random_hash digest_size i )
    in
    (hashes_0, hashes_1)

  let times = times hashes_0 hashes_1 tests

  let times_eqaf = List.filter is_eqaf times |> List.map snd

  let times_eqst = List.filter is_eqst times |> List.map snd

  let times_eqml = List.filter is_eqml times |> List.map snd

  let times_eqnt = List.filter is_eqnt times |> List.map snd

  let times_eqbg = List.filter is_eqbg times |> List.map snd
end

module E (Clock : CLOCK) (Perf : PERF) = struct
  module Make = Make (Clock) (Perf)
  open Make

  let hashes_0, hashes_1 =
    let copy x = Bytes.to_string (Bytes.of_string x) in
    let hashes_0 = Array.init tests (random_hash digest_size) in
    let hashes_1 = Array.init tests (fun i -> copy hashes_0.(i)) in
    (hashes_0, hashes_1)

  let times = times hashes_0 hashes_1 tests

  let times_eqaf = List.filter is_eqaf times |> List.map snd

  let times_eqst = List.filter is_eqst times |> List.map snd

  let times_eqml = List.filter is_eqml times |> List.map snd

  let times_eqnt = List.filter is_eqnt times |> List.map snd

  let times_eqbg = List.filter is_eqbg times |> List.map snd
end

let mean = function
  | [] -> invalid_arg "mean: empty list"
  | lst ->
      let t, l =
        List.fold_left (fun (t, l) x -> (x +. t, l +. 1.)) (0., 0.) lst
      in
      t /. l

let median lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  Array.sort Pervasives.compare arr ;
  (arr.((len - 1) / 2) +. arr.(len / 2)) /. 2.

let variance lst =
  let n = float_of_int (List.length lst - 1) in
  let m = mean lst in
  let e = List.fold_left (fun a x -> a +. ((x -. m) *. (x -. m))) 0. lst in
  sqrt (1. /. n *. e)

let deviation lst = sqrt (variance lst)

let list_min_elt compare lst =
  List.fold_left
    (function
      | Some m -> fun x -> if compare x m < 0 then Some x else Some m
      | None -> fun x -> Some x)
    None lst

let list_max_elt compare lst =
  List.fold_left
    (function
      | Some m -> fun x -> if compare x m > 0 then Some x else Some m
      | None -> fun x -> Some x)
    None lst

let min lst =
  match list_min_elt Pervasives.compare lst with
  | Some v -> v
  | None -> invalid_arg "min: empty list"

let max lst =
  match list_max_elt Pervasives.compare lst with
  | Some v -> v
  | None -> invalid_arg "max: empty list"

let success = 0

let failure = 1

let info ~name times =
  let min = min times in
  let max = max times in
  let mean = mean times in
  let median = median times in
  let deviation = deviation times in
  let r = deviation *. mean /. 100. in
  Fmt.pr "---------- %s ----------\n%!" name ;
  Fmt.pr "min:       %f.\n%!" min ;
  Fmt.pr "max:       %f.\n%!" max ;
  Fmt.pr "mean:      %f.\n%!" mean ;
  Fmt.pr "median:    %f.\n%!" median ;
  Fmt.pr "deviation: %f.\n%!" deviation ;
  Fmt.pr "deviation: %f%%.\n%!" r

module Main (Clock : CLOCK) (Perf : PERF) = struct
  module E = E (Clock) (Perf)
  module R = R (Clock) (Perf)

  let main () =
    Fmt.pr "########## Random ##########\n%!" ;
    info ~name:"eqaf" R.times_eqaf ;
    info ~name:"eqst" R.times_eqst ;
    info ~name:"eqml" R.times_eqml ;
    info ~name:"eqnt" R.times_eqnt ;
    info ~name:"eqbg" R.times_eqbg ;
    Fmt.pr "########## Equal ##########\n%!" ;
    info ~name:"eqaf" E.times_eqaf ;
    info ~name:"eqst" E.times_eqst ;
    info ~name:"eqml" E.times_eqml ;
    info ~name:"eqnt" E.times_eqnt ;
    info ~name:"eqbg" E.times_eqbg ;
    let times_eqaf = List.map2 ( -. ) R.times_eqaf E.times_eqaf in
    let times_eqst = List.map2 ( -. ) R.times_eqst E.times_eqst in
    let times_eqml = List.map2 ( -. ) R.times_eqml E.times_eqml in
    let times_eqnt = List.map2 ( -. ) R.times_eqnt E.times_eqnt in
    let times_eqbg = List.map2 ( -. ) R.times_eqbg E.times_eqbg in
    Fmt.pr "########## Total ##########\n%!" ;
    info ~name:"eqaf" times_eqaf ;
    info ~name:"eqst" times_eqst ;
    info ~name:"eqml" times_eqml ;
    info ~name:"eqnt" times_eqnt ;
    info ~name:"eqbg" times_eqbg ;
    let r = deviation times_eqaf *. mean times_eqaf /. 100. in
    if (r >= -10. && r <= 10.) && r >= -10. && r <= 10. then exit success
    else exit failure
end

open Bench_linux_toolkit
module X = Main (Clock) (Perf)

let () = X.main ()
