type test =
  { mean : float array
  ; m2 : float array
  ; n : float array }

type config =
  { chunk_size : int
  ; number_measurements : int }

type 'a packed = V : ([ `Init ] -> 'a -> 'b) -> 'a packed

type 'a context =
  { ticks : int64 array
  ; exec_times : int64 array
  ; ttests : test array
  ; percentiles : int64 array
  ; classes : int array
  ; inputs_data : 'a array
  ; computation : 'a packed
  ; config : config }

(* XXX(dinosaure): Online Welch's t-test
 * Tests wether two populations have same mean.
 * This is basically Student's t-test for unequal
 * variances and unequal sample sizes. *)

let t_push test x population =
  test.n.(population) <- test.n.(population) +. 1. ;
  (* XXX(dinosaure): estimate variance on the fly as per the Welford
   * method. This gives good numerical stability, see Knuth's TAOCP vol 2. *)
  let delta = x -. test.mean.(population) in
  test.mean.(population) <- test.mean.(population) +. delta /. test.n.(population) ;
  test.m2.(population) <- test.m2.(population) +. delta *. (x -. test.mean.(population))
;;

let t_compute test =
  let var = [| 0.; 0. |] in
  var.(0) <- test.m2.(0) /. (test.n.(0) -. 1.) ;
  var.(1) <- test.m2.(1) /. (test.n.(1) -. 1.) ;
  let num = test.mean.(0) -. test.mean.(1) in
  let den = sqrt ((var.(0) /. test.n.(0)) +. (var.(1) /. test.n.(1))) in
  num /. den

let percentile a which =
  Array.fast_sort Int64.compare a ;
  let array_position = Float.of_int (Array.length a) *. which in
  let array_position = Float.to_int array_position in
  assert (array_position >= 0) ;
  assert (array_position < Array.length a) ;
  a.(array_position)

let _number_percentiles = 100
let _number_percentiles' = Float.of_int _number_percentiles

let prepare_percentile ctx =
  for i = 0 to _number_percentiles - 1 do
    ctx.percentiles.(i) <- percentile
      ctx.exec_times (1. -. (0.5 ** (10. *. (Float.of_int (succ i)) /. _number_percentiles'))) ;
  done
;;

external cpu_cycles : unit -> (int64[@unboxed]) = "" "caml_cpu_cycles" [@@noalloc]

let stabilize_gc () =
  let rec go fail last_heap_live_words =
    if fail <= 0 then failwith "Unable to stabilize the number of live words in the major heap." ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then go (pred fail) stat.Gc.live_words in
  go 10 0

let measure ctx =
  let V f = ctx.computation in
  let do_one_computation = f `Init in
  for i = 0 to ctx.config.number_measurements - 1 do
    ctx.ticks.(i) <- cpu_cycles () ;
    ignore (do_one_computation ctx.inputs_data.(i))
  done ;
  ctx.ticks.(ctx.config.number_measurements) <- cpu_cycles () ;
  for i = 0 to ctx.config.number_measurements - 2 do
    ctx.exec_times.(i) <- Int64.sub ctx.ticks.(i + 1) ctx.ticks.(i)
  done
;;

let update_statistics ctx =
  for i = 10 to ctx.config.number_measurements - 2 do
    let diff = ctx.exec_times.(i) in
    if diff >= 0L then begin
      t_push ctx.ttests.(0) (Int64.to_float diff) ctx.classes.(i) ;
      for crop_index = 0 to _number_percentiles - 1 do
        if diff < ctx.percentiles.(crop_index)
        then t_push ctx.ttests.(crop_index + 1) (Int64.to_float diff) ctx.classes.(i)
      done ;
      if (ctx.ttests.(0)).n.(0) > 10_000.
      then begin
        let centered = Int64.to_float diff -. (ctx.ttests.(0)).mean.(ctx.classes.(i)) in
        t_push ctx.ttests.(1 + _number_percentiles) (centered *. centered) ctx.classes.(i)
      end
    end
  done
;;

let _enough_measurements = 10_000
let _enough_measurements' = Float.of_int _enough_measurements
let _tests = 1 + _number_percentiles + 1

let max_test ctx =
  let ret = ref 0 in
  let max = ref 0. in
  for i = 0 to _tests - 1 do
    if (ctx.ttests.(i)).n.(0) > _enough_measurements'
    then begin
      let x = Float.abs (t_compute ctx.ttests.(i)) in
      if !max < x then begin max := x ; ret := i end
    end
  done ; ctx.ttests.(!ret)

let t_threshold_bananas = 500.
let t_threshold_moderate = 10.

type result = Leakage_found | No_leakage_evidence_yet

let report ctx =
  let t = max_test ctx in
  let max_t = Float.abs (t_compute t) in
  let number_traces_max_t = t.n.(0) +. t.n.(1) in
  let max_tau = max_t /. sqrt number_traces_max_t in
  Format.printf "meas: %7.2f M, " (number_traces_max_t /. 1e6) ;
  if number_traces_max_t < _enough_measurements'
  then begin
    Format.printf "not enough measurements (%.0f still to go).\n%!"
      (_enough_measurements' -. number_traces_max_t) ;
    No_leakage_evidence_yet
  end else begin 
    Format.printf "max t: %+7.2f, max tau: %.2e, (5/tau)^2: %.2e."
      max_t max_tau (25. /. (max_tau *. max_tau)) ;
    if max_t > t_threshold_bananas
    then ( Format.printf " Definitely not constant time.\n%!" ; Leakage_found )
    else if max_t > t_threshold_moderate
    then ( Format.printf " Probably not constant time.\n%!" ; Leakage_found )
    else (* if max_t < t_threshold_moderate *)
    ( Format.printf " For the moment, may be constant time.\n%!" ; No_leakage_evidence_yet )
  end
;;

let main ctx =
  stabilize_gc () ; measure ctx ;
  let first_time = ctx.percentiles.(_number_percentiles - 1) = 0L in
  if first_time
  then ( prepare_percentile ctx ; No_leakage_evidence_yet )
  else begin
    update_statistics ctx ;
    report ctx
  end

let context ~prepare_inputs cfg computation =
  let classes = Array.make cfg.number_measurements 0 in
  { config= cfg
  ; ticks= Array.make (succ cfg.number_measurements) 0L
  ; exec_times= Array.make cfg.number_measurements 0L
  ; classes
  ; inputs_data= prepare_inputs cfg.number_measurements cfg.chunk_size classes
  ; ttests= Array.init _tests (fun _ -> { mean= [| 0.; 0. |]; m2= [| 0.; 0. |]; n= [| 0.; 0.; |] })
  ; computation
  ; percentiles= Array.make _number_percentiles 0L }
