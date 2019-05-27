open Crowbar

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

let () =
  add_test ~name:"compare_le" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare a b in
  let result = Eqaf.compare_be a b in
  check_eq ~pp:Format.pp_print_int ~eq:(=) expect result

let () =
  add_test ~name:"compare_be" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare (rev a) (rev b) in
  let result = Eqaf.compare_le a b in
  check_eq ~pp:Format.pp_print_int ~eq:(=) expect result
