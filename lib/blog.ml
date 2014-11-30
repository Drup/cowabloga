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

open Site

type blog = {
  path : string ; (* relative path *)
  entries : entry list ;
}

and entry = {
  date: Date.t;
  authors: Syndic.Atom.author list;
  title: string;
  file: string;
  body: Html5_types.div Html.elt;
}

(** An Atom feed: metadata plus a way to retrieve entries. *)
(** A feed is made up of Entries. *)
module Entry = struct

  (** An entry in a feed: metadata plus a filename [body]. *)
  type t = entry

  (** [permalink feed entry] returns the permalink URI for [entry] in [feed]. *)
  let permalink blog entry =
    Printf.sprintf "%s%s" blog.path entry.file

  (** Compare two entries. *)
  let compare a b = Date.compare b.date a.date

  (** [to_html feed entry] converts a blog entry in the given feed into an
      Html.t fragment. *)
  let to_html blog entry =
    let content = entry.body in
    let permalink_disqus =
      Printf.sprintf "%s%s#disqus_thread" blog.path entry.file
    in
    let title =
      let permalink = Uri.of_string (permalink blog entry) in
      entry.title, permalink
    in
    let content = (content : [`Div] Html.elt :> [>`Div] Html.elt) in
    Foundation.Blog.post ~title ~date:entry.date ~authors:entry.authors ~content

  (** [to_atom feed entry] *)
  let to_atom config feed entry =
    let links = [
      Syndic.Atom.link ~rel:Alternate ~type_media:"text/html"
        (Uri.of_string (permalink feed entry))
    ] in
    Syndic.Atom.entry
      ~id:(permalink feed entry)
      ~title:(Text entry.title)
      ~authors:(List.hd entry.authors, List.tl entry.authors) (*TOFIX*)
      ~updated:(Date.to_cal entry.date)
      ?rights:config.rights
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
    | [] -> []
    | hd::tl ->
      let hd = Entry.to_html blog hd in
      let tl = concat tl in
      hd :: sep :: tl
  in
  concat (List.sort Entry.compare blog.entries)

let permalink blog = Uri.of_string @@ Printf.sprintf "%sindex.html" blog.path

let feed_uri blog = Uri.of_string @@ Printf.sprintf "%satom.xml" blog.path

(** [to_atom feed entries] generates a time-ordered ATOM RSS [feed] for a
    sequence of [entries]. *)
let to_atom ({ authors ; rights ; title ; subtitle } as config) blog =
  let mk_uri x = Uri.of_string (blog.path ^ x) in

  let entries = List.sort Entry.compare blog.entries in
  let updated = Date.to_cal (List.hd entries).date in
  let links = [
    Syndic.Atom.link ~rel:Self @@ feed_uri blog ;
    Syndic.Atom.link ~rel:Alternate ~type_media:"text/html" @@ permalink blog ;
  ] in
  let entries = List.map (Entry.to_atom config blog) entries in
  Syndic.Atom.feed
    ~title:(Text title) (* ?subtitle *) ?rights ~updated ~links (* from config *)
    ~id:blog.path
    ~authors
    entries

(** [recent_posts feed entries] . *)
let recent_posts ?(active="") blog =
  let entries = List.sort Entry.compare blog.entries in
  List.map (fun e ->
      let link = (e.title, Uri.of_string (Entry.permalink blog e)) in
      if e.title = active then
        `active_link link
      else
        `link link
    ) entries
