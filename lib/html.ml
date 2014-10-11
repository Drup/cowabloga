
open Html5

include M
include P

let print_to_string f x =
  let b = Buffer.create 17 in
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  f encode (Buffer.add_string b) x ;
  Buffer.contents b

let to_string =
  print_to_string (fun encode output x -> P.print_list ~encode ~output [x])

let doc_to_string =
  print_to_string (fun encode output x -> P.print ~encode ~output x)

let to_text x =
  Syndic.Atom.Html (to_string x)
