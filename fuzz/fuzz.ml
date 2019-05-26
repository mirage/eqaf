open Crowbar

let () =
  add_test ~name:"equal" [ bytes; bytes; ] @@ fun a b ->
  let expect = String.equal a b in
  let result = Eqaf.equal a b in
  check_eq ~pp:Format.pp_print_bool ~eq:(=) expect result
