(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

(** Generate an aggregated link feed from all the other feeds *)
open Lwt
open Syndic
open Config

(* let feed_icon = *)
(*   function *)
(*   | `Blog _ -> "fa-comment" *)
(*   | `Wiki _ -> "fa-book" *)
(*   | `Links _ -> "fa-external-link" *)

(*   | `Blog f -> f.Atom.id ^ "blog/" (\* TODO: Proper URL routing *\) *)
(*   | `Wiki f -> f.Atom.id ^ "wiki/" *)
(*   | `Links f -> f.Atom.id ^ "links/" *)



let aggregate ?(name="updates/atom.xml") ~config site =
  let l =
    List.fold_left (fun l -> function
      | `Blog blog ->
          (Some (Uri.of_string blog.Blog.path), Blog.to_atom ~config ~blog) :: l
      | `Wiki wiki ->
          (Some (Uri.of_string wiki.Wiki.path), Wiki.to_atom ~config ~wiki) :: l
      | `Links links ->
          (Some (Uri.of_string links.Links.path), Links.to_atom ~config ~links) :: l
      | _ -> l
    ) [] site
  in
  Atom.aggregate
    ~id:(config.base_uri ^ name)
    ~title:(Text config.title)
    (* ?subtitle:config.subtitle *)
    l


(* let to_html ?limit feeds = *)
(*   let open Atom in *)
(*   to_atom_entries feeds *)
(*   >|= List.mapi (fun i ({entry}, info) -> *)
(*     let fa = Printf.sprintf "fa-li fa %s" (feed_icon info) in *)
(*     (\* Find an alternate HTML link *\) *)
(*     try *)
(*       (match limit with |Some x when i > x -> raise Not_found |_ -> ()); *)
(*       let uri = *)
(*         let l = List.find (fun l -> l.rel = `alternate && l.typ = Some "text/html") entry.links in *)
(*         l.href in *)
(*       let (y,m,d,_,_) = entry.updated in *)
(*       let date = Printf.sprintf "(%d %s %d)" d (Date.short_string_of_month m) y in *)
(*       Html.[ *)
(*         li [a ~a:[a_href "feed_uri info"]] [i ~a:[a_class [fa]] []] ; *)
(*         a ~a:[a_href @@ Uri.to_string uri] [pcdata entry.title] ; *)
(*         i ~a:[a_class ["front_date"]] [pcdata date] *)
(*       ] *)
(*     with Not_found -> []) *)
(*   >|= fun fs -> Html.(ul ~a:[a_class ["fa-ul"]] fs) *)
