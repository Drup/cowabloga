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

(** Link stream *)
type stream = {
  name: string;
  icon: string;
}

(* Individual link *)
type t = {
  author: Atom.author ;
  id: string;
  uri: Uri.t;
  title: string;
  date: Date.date;
  stream: stream;
}

let permalink config l =
  sprintf "%slinks/%s/%s" config.Config.base_uri l.stream.name l.id

let atom_entry_of_link config l =
  let perma_uri = Uri.of_string (permalink config l) in
  let links = [
   (*  Atom.mk_link ~rel:`alternate ~typ:"text/html" perma_uri; *)
    Atom.mk_link ~rel:Alternate ~type_media:"text/html" l.uri;
  ] in
  let content =
    Html.(span [
      a ~a:[a_href @@ Uri.to_string l.uri] [pcdata l.title] ;
      pcdata ", from" ; pcdata l.stream.name ;
    ])
  in
  Lwt.return @@
  Atom.mk_entry
    ~id:(Uri.to_string perma_uri)
    ~title:(Text l.title)
    ~updated:(atom_date l.date)
    ~authors:(l.author, [])
    ?rights:config.rights
    ~links
    ~content:(Html.to_text content)
    ()

let cmp_ent a b =
  compare (atom_date a.date) (atom_date b.date)

let to_atom ~config ~entries =
  let {Config. title; subtitle; base_uri; id; rights; authors } = config in
  let mk_uri x = Uri.of_string (base_uri ^ x) in
  let es = List.rev (List.sort cmp_ent entries) in
  let updated = atom_date (List.hd es).date in
  let id = base_uri ^ "links/" in
  let links = [
    Atom.mk_link ~rel:Alternate (mk_uri "atom.xml");
    Atom.mk_link ~rel:Alternate ~type_media:"text/html" (mk_uri "")
  ] in
  lwt entries = Lwt_list.map_s (atom_entry_of_link config) es in
  Lwt.return @@
  Atom.mk_feed
    ~id ~title ?subtitle
    ?rights ~updated ~links
    ~authors
    entries
