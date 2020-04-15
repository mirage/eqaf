type t =
  { heap : Bigstringaf.t
  ; mutable brk : int }

let make ~len =
  { heap= Bigstringaf.create len
  ; brk= 0 }

let size_of_word = Sys.word_size / 8

let needed_string str =
  let len = (String.length str + size_of_word) / size_of_word in
  ((len + 1) * size_of_word)

let inject_string ({ heap; brk; } as t) str =
  let len = (String.length str + size_of_word) / size_of_word in
  Bigstringaf.set_int64_be heap brk Int64.(of_int ((len lsl 10) lor 252)) ;
  Bigstringaf.set_int64_be heap (brk + size_of_word + ((len - 1) * size_of_word)) 0L ;
  let offset = (len * size_of_word) - 1 in
  Bigstringaf.blit_from_string str ~src_off:0 heap ~dst_off:(brk + size_of_word) ~len:(String.length str) ;
  Bigstringaf.set heap (brk + size_of_word + offset) (Char.chr (offset - String.length str)) ;
  t.brk <- brk + size_of_word + (len * size_of_word) ;
  brk + size_of_word


