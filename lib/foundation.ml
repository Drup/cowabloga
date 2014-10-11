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

  let top_nav ?(align=`Right) (links:links) =
    let links = List.map link links in
    let cl = match align with `Right -> "right" | `Left -> "left" in
    mk_ul_links ~cl ~links

  let button_group (links:links) =
    let links = List.map (link ~cl:["button"]) links in
    mk_ul_links ~cl:"button-group" ~links

  let side_nav (links:links) =
    let links = List.map link links in
    mk_ul_links ~cl:"side-nav" ~links

  let bottom_nav (links:links) =
    let links = List.map link links in
    mk_ul_links ~cl:"inline-list right" ~links
end

module Sidebar = struct
  type t = [
    | `link of Link.t
    | `active_link of Link.t
    | `divider
    | `text of string
    | `html of Html5_types.li_content Html.elt
  ]

  let t ~title:my_title ~content =
    let to_html (x:t) =
      match x with
        |`link l -> Html.(li [Link.link l])
        |`active_link l -> Html.(li ~a:[a_class ["active"]] [Link.link l])
        |`divider -> Html.(li ~a:[a_class ["divider"]] [])
        |`html h -> Html.(li [h])
        |`text t -> Html.(li [pcdata t])
    in
    Html.[
      h5 [pcdata my_title] ;
      ul ~a:[a_class ["side-nav"]] (List.map to_html content)
    ]
end

module Index = struct
  let t ~top_nav =
    let content =
      Html.[
        top_nav ;
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
          let a_nodes = intercalate (Html.pcdata ", ") (List.map link authors) in
          Html.(pcdata "By " :: a_nodes)
    in
    let title_text, title_uri = title in
    Html.(
      article [
        date ;
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
          content ::
          script ~a:[a_src "/js/vendor/jquery.min.js"] (pcdata "") ::
          script ~a:[a_src "/js/foundation/foundation.min.js"] (pcdata "") ::
          script ~a:[a_src "/js/foundation/foundation.topbar.js"] (pcdata "") ::
          script (pcdata "$(document).foundation();") ::
          highlight_trailer @
          trailers
        ))
  )

let top_nav ~title:my_title ~title_uri ~nav_links =
  Html.(
    div ~a:[a_class ["contain-to-grid fixed"]] [
      nav ~a:[a_class ["top-bar"]] [
        ul ~a:[a_class ["title-area"]] [
          li ~a:[a_class ["name"]] [h1 [a ~a:[a_href title_uri] [pcdata my_title]]] ;
          li ~a:[a_class ["toggle-topbar menu-icon"]] [
            a ~a:[a_href "#"] [span [pcdata "Menu"]]]
        ] ;
        section ~a:[a_class ["top-bar-section"]] nav_links
      ]
    ]
  )

let page ~body =
(*   Printf.sprintf "\ *)
(* <!DOCTYPE html> *)
(*   <!--[if IE 8]><html class=\"no-js lt-ie9\" lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><![endif]--> *)
(*   <!--[if gt IE 8]><!--><html class=\"no-js\" lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><!--<![endif]--> *)
(*   %s *)
(* </html>"  *)
    (Html.to_string body)
