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
open Lwt
open Syndic
open Site


type wiki = {
  path : string ;
  entries : entry list ;
}

and entry = {
  updated    : Date.t;
  author     : Atom.author;
  subject    : string;
  body       : Html5_types.div Html.elt ;
  permalink  : string;
}

let html_of_author (author : Atom.author) =
  match author.Atom.uri with
  | None     ->
    Html.pcdata @@ ("Last modified by " ^ author.Atom.name)
  | Some uri ->
    Html.(span [
      pcdata @@ "Last modified by " ;
      a ~a:[a_href @@ Uri.to_string uri] [pcdata author.Atom.name] ;
      ])

let body_of_entry e =
  (e.body : [`Div] Html.elt :> [> `Div] Html.elt) (* meh. *)

let compare_dates e1 e2 = Date.compare e2.updated e1.updated

(* Convert a wiki record into an Html.t fragment *)
let html_of_entry ?(want_date=false) e =
  let my_entry = body_of_entry e in
  return @@ Html.[
    h3 [a ~a:[a_href e.permalink] [pcdata e.subject]] ;
    my_entry ;
  ]

(* let html_of_index config = *)
(*   lwt my_body = config.read_file "index.md" in (\* TODO: make customizable *\) *)
(*   return @@ Html.( *)
(*     div ~a:[a_class ["wiki_entry"]] [ *)
(*       div ~a:[a_class ["wiki_entry_body"]] [my_body] *)
(*     ]) *)

let permalink wiki e =
  sprintf "%s%s" wiki.path e.permalink

let html_of_recent_updates wiki id (entries:entry list) =
  let ents = List.rev (List.sort compare_dates entries) in
  let html_of_ent e =
    Html.(
      li [
        a ~a:[a_href @@ permalink wiki e] [pcdata e.subject] ;
        span ~a:[a_class ["lastmod"]] (Date.to_short_html e.updated)
      ])
  in
  Html.[
    h6 [pcdata "Recent Updates"] ;
    ul ~a:[a_class ["side-nav"]] @@ List.map html_of_ent ents
  ]

(* Main wiki page; disqus comments are for full entry pages *)
let html_of_page ?disqus ~content ~sidebar =

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

  lwt content = content in
  let sidebar =
    match sidebar with
      | [] -> []
      | sidebar ->
          Html.[aside ~a:[a_class ["medium-3 large-3 columns panel"]] sidebar]
  in
  return Html.(
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

let permalink_exists x entries =
  List.exists (fun e -> e.permalink = x) entries

let index_link wiki = sprintf "%s/index.html" wiki.path

let atom_entry_of_ent config wiki e =
  let perma_uri = Uri.of_string (permalink wiki e) in
  let links =
    [ Atom.link ~rel:Alternate ~type_media:"text/html" perma_uri ] in
  let content = body_of_entry e in
  Atom.entry
    ~id:(Uri.to_string perma_uri)
    ~title:(Text e.subject)
    ~authors:(e.author, [])
    ~updated:(Date.to_cal e.updated)
    ?rights:config.rights
    ~content:(Html (Html.to_string content))
    ~links
    ()

let to_atom ~config ~wiki =
  let { title; subtitle; base_uri; rights } = config in
  let mk_uri x = Uri.of_string (wiki.path ^ x) in

  let es = List.rev (List.sort compare_dates wiki.entries) in
  let updated = Date.to_cal (List.hd es).updated in
  let links = [
    Atom.link ~rel:Self (mk_uri "atom.xml");
    Atom.link ~rel:Alternate ~type_media:"text/html" (mk_uri "")
  ] in
  let entries = List.map (atom_entry_of_ent config wiki) es in
  Atom.feed
    ~id:wiki.path ~title:(Text title) (* ?subtitle *)
    ?rights ~updated ~links
    ~authors:config.authors
    entries
