open Crowbar

let () =
  add_test ~name:"select_a_if_in_range"
    [ range max_int ; range max_int ; int; range 10000; range 10000 ]
  @@ fun low high n a b ->
  let low, high = min low high, max low high in
  let choice = Eqaf.select_a_if_in_range ~low ~high ~n a b in
  check_eq ~eq:(=) (if low <= n && n <= high then a else b) choice

let () =
  add_test ~name:"divmod" [ int32 ; int32 ]
  @@ fun x m ->
  try
    let result = Eqaf.divmod ~x ~m in
    let expect = Int32.unsigned_div x m,
                 Int32.unsigned_rem x m in
    check_eq ~eq:(=) expect result
  with
  | Invalid_argument desc ->
    (* we expect this for negative m: *)
    assert (desc = "m <= 0" || desc = "m >= 16348 not supported")

let () =
  add_test ~name:"hex_of_string |> string_of_hex" [ bytes ]
  @@ fun raw ->
  let enc = Eqaf.hex_of_string raw in
  let dec = Eqaf.string_of_hex enc in
  check_eq ~pp:(fun fmt s -> Format.pp_print_string fmt @@ String.escaped s)
    ~eq:(=) raw dec ;
  String.iter (function (* check for invalid encoding: *)
      | '0'..'9'
      | 'a'..'z'
      | 'A'..'Z' -> ()
      | _ -> assert false) enc

let () =
  add_test ~name:"string_of_hex |> hex_of_string" [ bytes ]
  @@ fun hex ->
  begin
    try begin
      let dec = Eqaf.string_of_hex hex in
      let enc = Eqaf.hex_of_string dec in
      check_eq ~pp:(fun fmt s -> Format.pp_print_string fmt @@ String.escaped s)
        ~eq:(=) (String.lowercase_ascii hex) enc ;
    end
    with Invalid_argument x ->
      let invalid = ref false in
      begin
        if (String.length hex mod 2 = 1)
        && x = "hex length must be multiple of 2" then begin
          invalid := true
        end else begin if x = "decoding invalid hex" then
            String.iter (function
                | 'a'..'f'
                | '0'..'9'
                | 'A'..'F' -> ()
                | _ -> invalid := true
              ) hex ;
        end ;
      end ; assert !invalid (* we expect it to be invalid since it raised *)
  end

let () =
  add_test ~name:"equal" [ bytes; bytes; ] @@ fun a b ->
  let expect = String.equal a b in
  let result = Eqaf.equal a b in
  check_eq ~pp:Format.pp_print_bool ~eq:(=) expect result

let rev str =
  let len = String.length str in
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res (len - 1 - i) str.[i] done ;
  Bytes.unsafe_to_string res

type order = Zero | Neg | Pos

let of_int = function 0 -> Zero | n -> if n < 0 then Neg else Pos

let pf = Format.fprintf
let pp_order ppf = function Zero -> pf ppf "Zero" | Neg -> pf ppf "Neg" | Pos -> pf ppf "Pos"

let () =
  add_test ~name:"compare_le" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare a b in
  let result = Eqaf.compare_be a b in
  check_eq ~pp:pp_order ~eq:(=) (of_int expect) (of_int result)

let () =
  add_test ~name:"compare_be" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare (rev a) (rev b) in
  let result = Eqaf.compare_le a b in
  check_eq ~pp:pp_order ~eq:(=) (of_int expect) (of_int result)
