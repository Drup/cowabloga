
let from_lexbuf =
  let open Sedlexing in

  let delim = [%sedlex.regexp? Plus '-'] in

  let rec start lexbuf = match%sedlex lexbuf with
    | delim ->
        get_inside
          (Buffer.create 17)
          (lexeme_length lexbuf)
          lexbuf
    | white_space -> start lexbuf
    | _ -> get_end "" lexbuf

  and get_inside buf i lexbuf = match%sedlex lexbuf with
    | delim ->
        if lexeme_length lexbuf = i
        then get_end (Buffer.contents buf) lexbuf
        else begin
          Buffer.add_string buf (Utf8.lexeme lexbuf) ;
          get_inside buf i lexbuf ;
        end
    | _ -> begin
        Buffer.add_string buf (Utf8.lexeme lexbuf) ;
        get_inside buf i lexbuf ;
      end

  and get_end content lexbuf = match%sedlex lexbuf with
    | Star any, eof -> (content, Utf8.lexeme lexbuf)
    | _ -> assert false

  in
  fun lexbuf ->
    let (toml, omd) = start lexbuf in
    (Toml.Parser.from_string toml,
     Lambdoc_read_markdown.Simple.ambivalent_from_string omd)


let from_file file =
  let chan = open_in file in
  let res = from_lexbuf @@ Sedlexing.Utf8.from_channel chan in
  close_in chan ;
  res

let from_string s =
  from_lexbuf @@ Sedlexing.Utf8.from_string s
