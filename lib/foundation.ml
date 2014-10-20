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

module Link = struct
  type t = string * Uri.t
  type links = t list

  let link ?(cl=[]) (txt,uri) =
    Html.(a ~a:[a_href @@ Uri.to_string uri; a_class cl] [pcdata txt])

  let mk_ul_links ~cl ~links =
    let items = List.map (fun x -> Html.li [x]) links in
    Html.(ul ~a:[a_class [cl]] items)

end

module Nav = struct
  open Link

  type 'a t = ([
    | `Li of Link.t
    | `Ul of Link.t * 'a list
  ] as 'a)

  let of_content (content : Site.content list) : 'a t list =
    let aux_page = function
      | `File (name, file) -> (name, Uri.of_string file)
      | `Html (name, gen) -> (name, Uri.of_string name)
    in
    let rec aux = function
      | #Site.page as page -> `Li (aux_page page)
      | `Links (name, file) -> `Li (name, Uri.of_string file)
      | `Link (name, uri) -> `Li (name, uri)
      | `Blog (name, dir)
      | `Wiki (name, dir) -> `Li (name, Uri.of_string dir)
      | `Menu (index, list) ->
          let main = match index with
            | #Site.page as page -> aux_page page
            | `Cat name -> (name, Uri.of_string "#")
          in
          `Ul (main, List.map aux list)
    in List.map aux content

  let rec make_li_nav : 'a t -> _ = function
    | `Li link -> Html.li [Link.link link]
    | `Ul ((name, href), list) ->
        Html.(li ~a:[a_class ["has-dropdown"]] [
            a ~a:[a_href @@ Uri.to_string href] [pcdata name] ;
            ul (List.map make_li_nav list)
          ])

  let mk_ul_nav ~cl ~items = Html.(ul ~a:[a_class [cl]] items)

  let top ?(align=`Right) (nav:'a t list) =
    let items = List.map make_li_nav nav in
    let cl = match align with `Right -> "right" | `Left -> "left" in
    mk_ul_nav ~cl ~items

  let side (nav:'a t list) =
    let items = List.map make_li_nav nav in
    mk_ul_nav ~cl:"side-nav" ~items

  let bottom (nav:'a t list) =
    let items = List.map make_li_nav nav in
    mk_ul_nav ~cl:"inline-list right" ~items

end

module Button = struct

  let group (links:Link.t list) =
    let links = List.map (Link.link ~cl:["button"]) links in
    Link.mk_ul_links ~cl:"button-group" ~links

end

module Sidebar = struct
  type t = [
    | `link of Link.t
    | `active_link of Link.t
    | `divider
    | `text of string
    | `html of Html5_types.li_content Html.elt list
  ]

  let t ~title:my_title ~content =
    let to_html (x:t) =
      match x with
        |`link l -> Html.(li [Link.link l])
        |`active_link l -> Html.(li ~a:[a_class ["active"]] [Link.link l])
        |`divider -> Html.(li ~a:[a_class ["divider"]] [])
        |`html h -> Html.(li h)
        |`text t -> Html.(li [pcdata t])
    in
    Html.[
      h5 [pcdata my_title] ;
      ul ~a:[a_class ["side-nav"]] (List.map to_html content)
    ]
end

module Index = struct
  let t =
    let content =
      Html.[
        br () ;
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["large-12 columns"]] [
            img ~alt:"" ~src:"http://placehold.it/1000x400&amp;text=img" () ;
            hr ()
          ]
        ]
      ]
    in
    content
end

let rec intercalate x = function
  | []    -> []
  | [e]   -> [e]
  | e::es -> e :: x :: intercalate x es

module Blog = struct
  let post ~title ~authors ~date ~content =
    let open Link in
    let author = match authors with
      | [] -> []
      | _  ->
          let a_nodes = intercalate (Html.pcdata ", ") (List.map Person.to_html authors) in
          Html.(pcdata "By " :: a_nodes)
    in
    let title_text, title_uri = title in
    Html.(
      article [
        Date.to_html date ;
        h4 [a ~a:[a_href @@ Uri.to_string title_uri] [pcdata title_text]] ;
        p [i author] ;
        content
      ]
    )

  let t ~title:title_ ?subtitle ~sidebar ~posts ~copyright () =
    let subtitle =
      match subtitle with
        | None -> []
        | Some s -> Html.[small [pcdata s]]
    in
    Html.[
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["large-9 columns"]] [
          h2 (pcdata title_ :: subtitle)
        ]
      ] ;
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["small-12"; "large-9"; "columns"]] posts ;
        aside ~a:[a_class ["small-12"; "large-3"; "columns"; "panel"]] sidebar
      ] ;
      footer ~a:[a_class ["row"]] [
        div ~a:[a_class ["large-12"; "columns"]] [
          hr () ;
          div ~a:[a_class ["row"]] [
            div ~a:[a_class ["large-6"; "columns"]] [
              p [small [entity "copy"; pcdata " Copyright" ; copyright]]
            ]
          ]
        ]
      ]
    ]
end

let body ?google_analytics ?highlight
    ~title:my_title ~headers ~content ~trailers () =

  let highlight_css, highlight_trailer = match highlight with
    | None -> [], []
    | Some my_style ->
        Html.[link ~rel:[`Stylesheet] ~href:my_style ()],
        Html.[
            script ~a:[a_src "/js/vendor/highlight.pack.js"] (pcdata "") ;
            script (pcdata "hljs.initHighlightingOnLoad();")
          ]
  in
  let ga =
    match google_analytics with
      | None -> []
      | Some (a,d) -> Html.[
         script ~a:[a_mime_type "text/javascript"]
           (pcdata "//<![CDATA[
           var _gaq = _gaq || [];
           _gaq.push(['_setAccount', '$[`Data a]$']);
           _gaq.push(['_setDomainName', '$[`Data d]$']);
           _gaq.push(['_trackPageview']);

           (function() {
              var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
              ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
              var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
            })();
           //]]>")
        ]
  in
  Html.(
    html
      (head (title (pcdata my_title)) (
          meta ~a:[a_charset "utf-8"] () ::
          meta ~a:[a_name "viewport"; a_content "width=device-width"] () ::
          link ~rel:[`Stylesheet] ~href:"/css/foundation.min.css" () ::
          link ~rel:[`Stylesheet] ~href:"/css/site.css" () ::
          script ~a:[a_src "/js/vendor/custom.modernizr.js"] (pcdata "") ::
          highlight_css @
          ga @
          headers
        ))
      (body (
          content @
          script ~a:[a_src "/js/vendor/jquery.js"] (pcdata "") ::
          script ~a:[a_src "/js/foundation.min.js"] (pcdata "") ::
          script ~a:[a_src "/js/foundation/foundation.topbar.js"] (pcdata "") ::
          script (pcdata "$(document).foundation();") ::
          highlight_trailer @
          trailers
        ))
  )

let a_data_topbar s = Html.Unsafe.string_attrib "data-topbar" s

let top_nav_raw ~title:my_title ~title_uri ~nav_links =
  Html.(
    div ~a:[a_class ["contain-to-grid fixed"]] [
      nav ~a:[a_class ["top-bar"] ; a_data_topbar ""] [
        ul ~a:[a_class ["title-area"]] [
          li ~a:[a_class ["name"]] [h1 [a ~a:[a_href title_uri] [pcdata my_title]]] ;
          li ~a:[a_class ["toggle-topbar menu-icon"]] [
            a ~a:[a_href "#"] [span [pcdata "Menu"]]]
        ] ;
        section ~a:[a_class ["top-bar-section"]] nav_links
      ]
    ]
  )

let top_nav ?(title_uri="/") (`Menu (page, categories)) =
  match page with
    | `File (title, _) | `Html (title, _) ->
        top_nav_raw ~title ~title_uri
          ~nav_links:[Nav.(top ~align:`Left @@ of_content categories)]


let page ~body =
(*   Printf.sprintf "\ *)
(* <!DOCTYPE html> *)
(*   <!--[if IE 8]><html class=\"no-js lt-ie9\" lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><![endif]--> *)
(*   <!--[if gt IE 8]><!--><html class=\"no-js\" lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><!--<![endif]--> *)
(*   %s *)
(* </html>"  *)
    body
