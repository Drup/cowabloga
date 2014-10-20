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

module Link : sig
  type t = string * Uri.t
  type links = t list

  val link : ?cl:string list -> t -> [> `A of [> `PCDATA ] ] Html.elt
end

module Button : sig
  val group : Link.links -> [> Html5_types.ul ] Html.elt
end

module Nav : sig

  type 'a t = ([
    | `Li of Link.t
    | `Ul of Link.t * 'a list
  ] as 'a)

  val of_content : Site.content list -> 'a t list

  val top :
    ?align:[< `Left | `Right > `Right ] -> 'a t list -> [> Html5_types.ul ] Html.elt
  val side : 'a t list -> [> Html5_types.ul ] Html.elt
  val bottom : 'a t list -> [> Html5_types.ul ] Html.elt

end

module Sidebar : sig
  type t = [
    | `active_link of Link.t
    | `divider
    | `link of Link.t
    | `text of string
    | `html of Html5_types.li_content Html.elt list
  ]
  val t : title:string -> content:t list -> [> `H5 | `Ul ] Html.elt list
end

module Index : sig
  val t: [> `Br | `Div ] Html.elt list
end

module Blog : sig
  val post:
    title:string * Uri.t -> authors:Person.t list -> date:Date.t ->
    content:[< Html5_types.article_content_fun > `Div `H4 `P ]  Html.elt ->
    [> Html5_types.article ] Html.elt

  val t:
    title:string ->
    ?subtitle:string ->
    sidebar:[< Html5_types.aside_content_fun ] Html.elt list ->
    posts:[< Html5_types.div_content_fun ] Html.elt list ->
    copyright:[< Html5_types.small_content_fun > `PCDATA ] Html.elt ->
    unit -> [> `Div | `Footer ] Html.elt list

end

val body:
  ?google_analytics:(string * string) -> ?highlight:string -> title:string ->
  headers:Html5_types.head_content_fun Html.elt list ->
  content:([< Html5_types.body_content_fun > `Script ] as 'c) Html.elt list ->
  trailers:'c Html.elt list -> unit -> [> `Html ] Html.elt

val top_nav_raw :
  title:string -> title_uri:string ->
  nav_links:[< Html5_types.section_content_fun ] Html.elt list ->
  [> Html5_types.div ] Html.elt

val top_nav :
  ?title_uri:string ->
  [< `Menu of [< `Html of string * 'a | `File of string * 'b ] * Site.content list ] ->
  [> `Div] Html.elt

val page: body:'a Html5.M.elt -> 'a Html5.M.elt
