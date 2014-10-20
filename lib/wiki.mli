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

(** Wiki management: entries, ATOM feeds, etc. *)

open Syndic

type wiki = {
  path : string;
  entries : entry list;
}

and entry = {
  date : Date.t;
  author : Person.t;
  title : string;
  file : string;
  body : Html5_types.div Html.elt;
}


module Entry : sig
  type t = entry
  val permalink : wiki -> entry -> string
  val compare : entry -> entry -> int
  val to_html :
    ?want_date:bool -> wiki -> entry -> [> `Div | `H3 ] Html.elt list
  val to_atom : Site.config -> wiki -> entry -> Syndic.Atom.entry
end

val permalink : wiki -> Uri.t
val feed_uri : wiki -> Uri.t

val to_html :
  ?disqus:string ->
  content:[< Html5_types.div_content_fun ] Html.elt Html.list_wrap ->
  sidebar:[< Html5_types.aside_content_fun ] Html.elt Html.list_wrap ->
  [> `Aside | `Div ] Html.elt list

val to_atom : config:Site.config -> wiki:wiki -> Syndic.Atom.feed

val recent_updates : wiki -> Foundation.Sidebar.t list
