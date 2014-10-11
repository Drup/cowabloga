
open Html5

include M
include P

let to_string x =
  let b = Buffer.create 17 in
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  Html5.P.print_list ~encode ~output:(Buffer.add_string b) [x] ;
  Buffer.contents b

let to_text x =
  Syndic.Atom.Html (to_string x)
