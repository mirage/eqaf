let secret = "\001\002\003\004\005\006\042"

let do_one_computation v = Eqaf.equal v secret

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    let code = Random.int 256 in
    Bytes.set res i (Char.chr code)
  done ; Bytes.unsafe_to_string res

let prepare_inputs number_measurements chunk_size classes =
  let random i =
    match classes.(i) <- Random.int 2 ; classes.(i) with
    | 0 -> random_string chunk_size
    | 1 -> String.make chunk_size '\000'
    | _ -> assert false in
  Array.init number_measurements random

let rec loop ctx = match Check.main ctx with
  | `Leakage_found -> exit 1
  | `No_leakage_evidence_yet -> loop ctx

let () =
  let config =
    { Check.chunk_size= String.length secret
    ; Check.number_measurements= 500 } in
  let ctx = Check.context ~prepare_inputs config
    (Check.V (fun `Init -> do_one_computation)) in
  loop ctx
