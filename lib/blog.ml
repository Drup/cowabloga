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

(** Blog management: entries, ATOM feeds, etc. *)

open Printf
open Lwt
open Syndic
open Config


type blog = {
  path : string ; (* complete path *)
  icon: string ;
  entries : entry list ;
}

and entry = {
  updated: Date.date;
  authors: Atom.author list;
  subject: string;
  permalink: string;
  body: [ `Div] Html.elt;
}

(** An Atom feed: metadata plus a way to retrieve entries. *)
(** A feed is made up of Entries. *)
module Entry = struct

  (** An entry in a feed: metadata plus a filename [body]. *)
  type t = entry

  (** [permalink feed entry] returns the permalink URI for [entry] in [feed]. *)
  let permalink blog entry =
    sprintf "%s%s" blog.path entry.permalink

  (** Compare two entries. *)
  let compare a b =
    compare (Date.atom_date b.updated) (Date.atom_date a.updated)

  (** [to_html feed entry] converts a blog entry in the given feed into an
      Html.t fragment. *)
  let to_html ~blog ~entry =
    let content = entry.body in
    let permalink_disqus =
      sprintf "%s%s#disqus_thread" blog.path entry.permalink
    in
    let authors =
      List.map (fun { Atom.name ; uri } ->
        let author_uri = match uri with
          | None -> Uri.of_string "" (* TODO *)
          | Some uri -> uri
        in
        name, author_uri)
      entry.authors
    in
    let date = Date.html_of_date entry.updated in
    let title =
      let permalink = Uri.of_string (permalink blog entry) in
      entry.subject, permalink
    in
    let content = (content : [`Div] Html.elt :> [>`Div] Html.elt) in
    Foundation.Blog.post ~title ~date ~authors ~content

  (** [to_atom feed entry] *)
  let to_atom feed entry =
    let links = [
      Atom.mk_link ~rel:Alternate ~type_media:"text/html"
        (Uri.of_string (permalink feed entry))
    ] in
    Atom.mk_entry
      ~id:(permalink feed entry)
      ~title:(Text entry.subject)
      ~authors:(List.hd entry.authors, List.tl entry.authors) (*TOFIX*)
      ~updated:(Date.atom_date entry.updated)
      ~links
      ~content:(Html.to_text entry.body)
      ()

end

(** Entries separated by <hr /> tags *)
let default_separator = Html.hr ()

(** [to_html ?sep feed entries] renders a series of entries in a feed, separated
    by [sep], defaulting to [default_separator]. *)
let to_html ?(sep=default_separator) blog =
  let rec concat = function
    | [] -> [Html.entity "&"]
    | hd::tl ->
      let hd = Entry.to_html blog hd in
      let tl = concat tl in
      hd :: sep :: tl
  in
  concat (List.sort Entry.compare blog.entries)


(** [to_atom feed entries] generates a time-ordered ATOM RSS [feed] for a
    sequence of [entries]. *)
let to_atom ~config:{Config. authors ; rights ; title ; subtitle } ~blog =
  let mk_uri x = Uri.of_string (blog.path ^ x) in

  let entries = List.sort Entry.compare blog.entries in
  let updated = Date.atom_date (List.hd entries).updated in
  let links = [
    Atom.mk_link ~rel:Alternate (mk_uri "atom.xml");
    Atom.mk_link ~rel:Alternate ~type_media:"text/html" (mk_uri "")
  ] in
  let entries = List.map (Entry.to_atom blog) entries in
  Atom.mk_feed
    ~title:(Text title) (* ?subtitle *) ?rights ~updated ~links (* from config *)
    ~id:blog.path
    ~authors
    entries

(** [recent_posts feed entries] . *)
let recent_posts ?(active="") blog entries =
  let entries = List.sort Entry.compare entries in
  List.map (fun e ->
      let link = Entry.(e.subject, Uri.of_string (Entry.permalink blog e)) in
      if e.subject = active then
        `active_link link
      else
        `link link
    ) entries
