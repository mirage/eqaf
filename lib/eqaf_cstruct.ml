let equal a b =
  Eqaf_bigstring.equal (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)
