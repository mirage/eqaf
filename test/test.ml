let le a b expected =
  let title = Fmt.strf "le %S %S = %d" a b expected in
  Alcotest.test_case title `Quick @@ fun () ->
  let expected' = String.compare a b in
  Alcotest.(check int) "result" (Eqaf.compare_le a b) expected ;
  Alcotest.(check int) "string.compare" (Eqaf.compare_le a b) expected'

let be a b expected =
  let title = Fmt.strf "be %S %S = %d" a b expected in
  Alcotest.test_case title `Quick @@ fun () ->
  Alcotest.(check int) "result" (Eqaf.compare_be a b) expected

let () =
  Alcotest.run "eqaf"
    [ "le", [ le "a" "a" 0
            ; le "a" "b" (-1)
            ; le "b" "a" 1
            ; le "aa" "ab" (-1)
            ; le "aaa" "aba" (-1)
            ; le "bbb" "abc" 1
            ; le "bbb" "bbc" (-1)
            ; le "bbb" "abb" 1 ]
    ; "be", [ be "a" "a" 0
            ; be "a" "b" (-1)
            ; be "b" "a" 1
            ; be "aa" "ab" (-1)
            ; be "aaa" "aba" (-1)
            ; be "bbb" "abc" (-1)
            ; be "bbb" "bbc" (-1)
            ; be "bbb" "abb" 1 ] ]
