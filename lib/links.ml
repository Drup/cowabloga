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
open Lwt
open Date
open Syndic
open Config

type t = {
  path : string ; (* complete path *)
  entries : link list ;
}

and link = {
  author: Person.t ;
  id: string;
  uri: Uri.t;
  title: string;
  date: Date.date;
  source : source ;
}

and source = {
  name : string ;
  icon : string ;
}

module Entry = struct

  type t = link

  let permalink list l =
    sprintf "%s%s" list.path l.id

  let compare a b =
    compare (atom_date a.date) (atom_date b.date)

  let to_atom config list l =
    let perma_uri = Uri.of_string (permalink list l) in
    let links = [
      (*  Atom.mk_link ~rel:`alternate ~typ:"text/html" perma_uri; *)
      Atom.mk_link ~rel:Alternate ~type_media:"text/html" l.uri;
    ] in
    let content =
      Html.(span [
          a ~a:[a_href @@ Uri.to_string l.uri] [pcdata l.title] ;
          pcdata ", from" ; pcdata l.source.name ;
        ])
    in
    Atom.mk_entry
      ~id:(Uri.to_string perma_uri)
      ~title:(Text l.title)
      ~updated:(atom_date l.date)
      ~authors:(l.author, [])
      ?rights:config.rights
      ~links
      ~content:(Html.to_text content)
      ()

end

let to_atom ~config ~links:list =
  let {Config. title; subtitle; base_uri; rights; authors } = config in
  let mk_uri x = Uri.of_string (base_uri ^ x) in
  let es = List.rev (List.sort Entry.compare list.entries) in
  let updated = atom_date (List.hd es).date in
  let id = base_uri ^ "links/" in
  let links = [
    Atom.mk_link ~rel:Alternate (mk_uri "atom.xml");
    Atom.mk_link ~rel:Alternate ~type_media:"text/html" (mk_uri "")
  ] in
  let entries = List.map (Entry.to_atom config list) es in
  Atom.mk_feed
    ~id ~title:(Text title) (* ?subtitle *)
    ?rights ~updated ~links
    ~authors
    entries
