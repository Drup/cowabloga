(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
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

(** File system location. *)
type directory = string
type file = string
type name = string

type page = [
  | `Page of name * file
  | `Html of name * (unit -> Html.doc)
]

type content = [
  | page
  | `Blog of name * directory
  | `Wiki of name * directory
  | `Links of name * file
  | `Link of name * Uri.t
  | `Menu of [ page | `Cat of name] * content list
]

type t = {
  title: string;
  subtitle: string option;
  base_uri: string;
  rights: Syndic.Atom.rights option;
  authors: Person.t list;
  read_file: string -> Html5_types.div Html.elt Lwt.t;
}
