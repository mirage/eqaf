let equal a b =
  let a' = Bytes.unsafe_to_string a in
  let b' = Bytes.unsafe_to_string b in
  Eqaf.equal a' b'

let compare_le_with_len ~len a b =
  let a' = Bytes.unsafe_to_string a in
  let b' = Bytes.unsafe_to_string b in
  Eqaf.compare_le_with_len ~len a' b'

let compare_le a b =
  let a' = Bytes.unsafe_to_string a in
  let b' = Bytes.unsafe_to_string b in
  Eqaf.compare_le a' b'

let compare_be_with_len ~len a b =
  let a' = Bytes.unsafe_to_string a in
  let b' = Bytes.unsafe_to_string b in
  Eqaf.compare_be_with_len ~len a' b'

let compare_be a b =
  let a' = Bytes.unsafe_to_string a in
  let b' = Bytes.unsafe_to_string b in
  Eqaf.compare_be a' b'

let find_uint8 ?off ~f b =
  let str = Bytes.unsafe_to_string b in
  Eqaf.find_uint8 ?off ~f str

let exists_uint8 ?off ~f b =
  let str = Bytes.unsafe_to_string b in
  Eqaf.exists_uint8 ?off ~f str
