(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf
open Syndic
open Site

type links = {
  path : string ;
  entries : link list ;
}

and link = {
  author: Person.t ;
  id: string;
  uri: Uri.t;
  title: string;
  date: Date.t;
  source : source ;
}

and source = {
  name : string ;
  icon : string ;
}

module Entry = struct

  type t = link

  let permalink list l =
    sprintf "%s#%s" list.path l.id

  let compare a b = Date.compare b.date a.date

  let to_atom config links l =
    let perma_uri = Uri.of_string (permalink links l) in
    let links = [
      (*  Atom.link ~rel:`alternate ~typ:"text/html" perma_uri; *)
      Atom.link ~rel:Alternate ~type_media:"text/html" l.uri;
    ] in
    let content =
      Html.(span [
          a ~a:[a_href @@ Uri.to_string l.uri] [pcdata l.title] ;
          pcdata ", from" ; pcdata l.source.name ;
        ])
    in
    Atom.entry
      ~id:(Uri.to_string perma_uri)
      ~title:(Text l.title)
      ~updated:(Date.to_cal l.date)
      ~authors:(l.author, [])
      ?rights:config.rights
      ~links
      ~content:(Html.to_text content)
      ()

end

let permalink links = Uri.of_string @@ links.path
let feed_uri links = Uri.of_string @@ sprintf "%satom.xml" links.path

let to_atom config links =
  let {Site. title; subtitle; base_uri; rights; authors } = config in
  let entries = List.sort Entry.compare links.entries in
  let updated = Date.to_cal (List.hd entries).date in
  let atom_links = [
    Atom.link ~rel:Self (feed_uri links);
    Atom.link ~rel:Alternate ~type_media:"text/html" (permalink links)
  ] in
  let entries = List.map (Entry.to_atom config links) entries in
  Atom.feed
    ~id:links.path ~title:(Text title) (* ?subtitle *)
    ?rights ~updated ~links:atom_links
    ~authors
    entries
