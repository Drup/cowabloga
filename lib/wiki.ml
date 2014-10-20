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

open Printf
open Syndic
open Site


type wiki = {
  path : string ;
  entries : entry list ;
}

and entry = {
  date   : Date.t;
  author : Atom.author;
  title  : string;
  file   : string;
  body   : Html5_types.div Html.elt ;
}

let html_of_author (author : Atom.author) =
  [ Html.pcdata "Last modified by " ; Person.to_html author ]


module Entry = struct

  type t = entry

  let body e =
    (e.body : [`Div] Html.elt :> [> `Div] Html.elt) (* meh. *)

  let permalink wiki e =
    sprintf "%s%s" wiki.path e.file

  let compare e1 e2 = Date.compare e2.date e1.date

  (* Convert a wiki record into an Html.t fragment *)
  let to_html ?(want_date=false) wiki e =
    let my_entry = body e in
    Html.[
      h3 [a ~a:[a_href @@ permalink wiki e] [pcdata e.title]] ;
      my_entry ;
    ]

  let to_atom config wiki e =
    let perma_uri = Uri.of_string (permalink wiki e) in
    let links =
      [ Atom.link ~rel:Alternate ~type_media:"text/html" perma_uri ] in
    let content = body e in
    Atom.entry
      ~id:(Uri.to_string perma_uri)
      ~title:(Text e.title)
      ~authors:(e.author, [])
      ~updated:(Date.to_cal e.date)
      ?rights:config.rights
      ~content:(Html (Html.to_string content))
      ~links
      ()

end


(* let html_of_index config = *)
(*   lwt my_body = config.read_file "index.md" in (\* TODO: make customizable *\) *)
(*   return @@ Html.( *)
(*     div ~a:[a_class ["wiki_entry"]] [ *)
(*       div ~a:[a_class ["wiki_entry_body"]] [my_body] *)
(*     ]) *)



(* Main wiki page; disqus comments are for full entry pages *)
let to_html ?disqus ~content ~sidebar =

  (* The disqus comment *)
  let disqus_html permalink =
    Html.[
      div ~a:[a_class ["wiki_entry_comments"]] [] ;
      div ~a:[a_id "disqus_thread"] [] ;
      script @@ pcdata @@
        Printf.sprintf
          "var disqus_identifer = '%s';
      (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
         dsq.src = 'http://openmirage.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
       })()"
          permalink
    ] in

  let dh = match disqus with
     | Some perm  -> disqus_html perm
     | None      -> [] in

  let sidebar =
    match sidebar with
      | [] -> []
      | sidebar ->
          Html.[aside ~a:[a_class ["medium-3 large-3 columns panel"]] sidebar]
  in
  Html.(
    div ~a:[a_class ["row"]] [
      div ~a:[a_class ["small-12"; "medium-10"; "large-9"; "columns"]] [
        h2 [pcdata "Documentation" ; small [pcdata " and guides"]]
      ]]
    ::
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["small-12"; "medium-10"; "large-9"; "columns"]]
          content
      ]
    ::
      sidebar
  )

let permalink wiki = Uri.of_string @@ sprintf "%sindex.html" wiki.path
let feed_uri wiki = Uri.of_string @@ sprintf "%satom.xml" wiki.path

let to_atom ~config ~wiki =
  let { title; subtitle; base_uri; rights } = config in

  let entries = List.sort Entry.compare wiki.entries in
  let updated = Date.to_cal (List.hd entries).date in
  let links = [
    Atom.link ~rel:Self (feed_uri wiki);
    Atom.link ~rel:Alternate ~type_media:"text/html" (permalink wiki)
  ] in
  let entries = List.map (Entry.to_atom config wiki) entries in
  Atom.feed
    ~id:wiki.path ~title:(Text title) (* ?subtitle *)
    ?rights ~updated ~links
    ~authors:config.authors
    entries

let recent_updates wiki =
  let entries = List.sort Entry.compare wiki.entries in
  let html_of_ent e =
    `html Html.[
        a ~a:[a_href @@ Entry.permalink wiki e] [pcdata e.title] ;
        span ~a:[a_class ["lastmod"]] (Date.to_short_html e.date)
      ]
  in
  List.map html_of_ent entries
