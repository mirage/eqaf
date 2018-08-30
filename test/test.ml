(* Vanilla scope. *)

let chr = Char.unsafe_chr

let code = Char.code

let epr = Format.eprintf

let pr = Format.printf

let tests = 100

let digest_size = 4096

let random_hash digest_size _ =
  let ic = open_in_bin "/dev/urandom" in
  let rs = really_input_string ic digest_size in
  close_in ic ; rs

let () = Random.self_init ()

external eqaf : string -> string -> bool = "eqaf"

external eqst : string -> string -> bool = "caml_string_equal"

external string_get : string -> int -> int = "%string_unsafe_get"

let eqml a b =
  let rt = ref 0 in
  for i = 0 to digest_size - 1 do
    rt := !rt lor (string_get a i lxor string_get b i)
  done ;
  !rt = 0

module StdBytes = Bytes
module StdArray = Array

let list_init n f =
  let rec go acc = function
    | 0 -> List.rev acc
    | n -> go (f n :: acc) (pred n)
  in
  go [] n

(* Core scope. *)

open Core_bench
open Core.Std [@@warning "-3"]
open Core

(* Random test *)

let bench_eqaf hashes_0 hashes_1 n =
  Bench.Test.create_indexed ~name:"eqaf" ~args:(list_init n pred)
  @@ fun i ->
  let a = hashes_0.(i) in
  let b = hashes_1.(i) in
  (Staged.stage [@warning "-3"]) (fun () -> eqaf a b)

let bench_eqst hashes_0 hashes_1 n =
  Bench.Test.create_indexed ~name:"eqst" ~args:(list_init n pred)
  @@ fun i ->
  let a = hashes_0.(i) in
  let b = hashes_1.(i) in
  (Staged.stage [@warning "-3"]) (fun () -> eqst a b)

let bench_eqml hashes_0 hashes_1 n =
  Bench.Test.create_indexed ~name:"eqml" ~args:(list_init n pred)
  @@ fun i ->
  let a = hashes_0.(i) in
  let b = hashes_1.(i) in
  (Staged.stage [@warning "-3"]) (fun () -> eqml a b)

let measurements hashes_0 hashes_1 =
  let run_config =
    Bench.Run_config.create ~time_quota:(Time.Span.of_sec 1.) ()
  in
  Bench.measure ~run_config
    [ bench_eqaf hashes_0 hashes_1 tests
    ; bench_eqst hashes_0 hashes_1 tests
    ; bench_eqml hashes_0 hashes_1 tests ]

let analyzes hashes_0 hashes_1 =
  List.map (measurements hashes_0 hashes_1) ~f:(fun m ->
      match
        Bench.analyze ~analysis_configs:[Bench.Analysis_config.nanos_vs_runs] m
      with
      | Ok m -> m
      | Error exn ->
          epr "Got an error: %s.\n%!" (Error.to_string_hum exn) ;
          raise (Error.to_exn exn) )

type kind = Eqaf | Eqst | Eqml

let kind_of_string x =
  match String.sub x 0 4 with
  | "eqaf" -> Eqaf
  | "eqst" -> Eqst
  | "eqml" -> Eqml
  | _ -> invalid_arg "kind_of_string"

let is_eqaf = function Eqaf, _ -> true | _ -> false

let is_eqst = function Eqst, _ -> true | _ -> false

let is_eqml = function Eqml, _ -> true | _ -> false

let times hashes_0 hashes_1 =
  List.map (analyzes hashes_0 hashes_1) ~f:(fun a ->
      let r = Analysis_result.regressions a in
      let c = Analysis_result.Regression.coefficients r.(0) in
      ( kind_of_string (Analysis_result.name a)
      , Analysis_result.Coefficient.estimate c.(0) ) )

module R = struct
  let hashes_0, hashes_1 =
    let same0 = random_hash digest_size 0 in
    let same1 = StdBytes.to_string (StdBytes.of_string same0) in
    assert (not (phys_equal same0 same1)) ;
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

  let times = times hashes_0 hashes_1

  let times_eqaf = List.filter times ~f:is_eqaf |> List.map ~f:snd

  let times_eqst = List.filter times ~f:is_eqst |> List.map ~f:snd

  let times_eqml = List.filter times ~f:is_eqml |> List.map ~f:snd
end

module E = struct
  let hashes_0, hashes_1 =
    let hashes_0 = Array.init tests (random_hash digest_size) in
    let hashes_1 = Array.init tests (fun i -> String.copy hashes_0.(i)) in
    (hashes_0, hashes_1)

  let times = times hashes_0 hashes_1

  let times_eqaf = List.filter times ~f:is_eqaf |> List.map ~f:snd

  let times_eqst = List.filter times ~f:is_eqst |> List.map ~f:snd

  let times_eqml = List.filter times ~f:is_eqml |> List.map ~f:snd
end

let mean = function
  | [] -> invalid_arg "mean: empty list"
  | lst ->
      let t, l =
        List.fold_left lst ~init:(0., 0.) ~f:(fun (t, l) x -> (x +. t, l +. 1.))
      in
      t /. l

let median lst =
  let arr = List.to_array lst in
  let len = Array.length arr in
  StdArray.sort Float.compare arr ;
  (arr.((len - 1) / 2) +. arr.(len / 2)) /. 2.

let variance lst =
  let n = Float.of_int (List.length lst - 1) in
  let m = mean lst in
  let e =
    List.fold_left lst ~init:0. ~f:(fun a x -> a +. ((x -. m) *. (x -. m)))
  in
  sqrt (1. /. n *. e)

let deviation lst = sqrt (variance lst)

let min lst =
  match List.min_elt ~compare:Float.compare lst with
  | Some v -> v
  | None -> invalid_arg "min: empty list"

let max lst =
  match List.max_elt ~compare:Float.compare lst with
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
  pr "---------- %s ----------\n%!" name ;
  pr "min:       %f.\n%!" min ;
  pr "max:       %f.\n%!" max ;
  pr "mean:      %f.\n%!" mean ;
  pr "median:    %f.\n%!" median ;
  pr "deviation: %f.\n%!" deviation ;
  pr "deviation: %f%%.\n%!" r

let () =
  pr "########## Random ##########\n%!" ;
  info ~name:"eqaf" R.times_eqaf ;
  info ~name:"eqst" R.times_eqst ;
  info ~name:"eqml" R.times_eqml ;
  pr "########## Equal ##########\n%!" ;
  info ~name:"eqaf" E.times_eqaf ;
  info ~name:"eqst" E.times_eqst ;
  info ~name:"eqml" E.times_eqml ;
  let times_eqaf = List.map2_exn R.times_eqaf E.times_eqaf ~f:Float.sub in
  let times_eqst = List.map2_exn R.times_eqst E.times_eqst ~f:Float.sub in
  let times_eqml = List.map2_exn R.times_eqml E.times_eqml ~f:Float.sub in
  pr "########## Total ##########\n%!" ;
  info ~name:"eqaf" times_eqaf ;
  info ~name:"eqst" times_eqst ;
  info ~name:"eqml" times_eqml ;
  let r = deviation times_eqaf *. mean times_eqaf /. 100. in
  if r >= -10. && r <= 10. then exit success else exit failure
