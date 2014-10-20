open Cowabloga
open Lwt
open Site

let read_file ent =
  match Mirage_entries.read ent with
  | None -> Html.(div [pcdata ""])
  | Some b -> Html.Unsafe.data @@ Omd.to_html @@ Omd.of_string b


let config = {
  base_uri="http://localhost:8081";
  title = "The Mirage Blog";
  subtitle = Some "on building functional operating systems";
  rights = Mirage_people.rights;
  authors = [];
}

let content =
  `Menu (`Html ("/Mirage", Html.div Foundation.Index.t),
    [
      `Blog ("Blog",  "/blog/");
      (* `Wiki ("Docs", "/docs/"); *)
      `Link ("API" , Uri.of_string "http://mirage.github.io/");
      `File ("Community", "/community");
      `File ("About", "/about.md");
    ]
  )

let top_nav =
  Foundation.top_nav content

let blog_index blog =
  let recent_posts = Blog.recent_posts blog in
  let sidebar = Foundation.Sidebar.t ~title:"Recent Posts" ~content:recent_posts in
  let copyright = Html.pcdata "Anil Madhavapeddy" in
  let {Site. title; subtitle } = config in
  Foundation.Blog.t ~title ?subtitle ~sidebar ~posts:(Blog.to_html blog) ~copyright ()

let blog_template blog entry =
  let recent_posts = Blog.recent_posts blog in
  let sidebar = Foundation.Sidebar.t ~title:"Recent Posts" ~content:recent_posts in
  let copyright = Html.pcdata "Anil Madhavapeddy" in
  let content = Blog.Entry.to_html blog entry in
  let {Site. title; subtitle } = config in
  Foundation.Blog.t ~title ?subtitle ~sidebar ~posts:[content] ~copyright ()


let decorate page =
  let content = match page with
    | `WikiEntry h | `Page h -> [ (h : [`Div] Html.elt :> [> `Div] Html.elt) ]
    | `BlogEntry (blog, e) -> blog_template blog e
    | `BlogIndex blog -> blog_index blog
    | _ -> assert false
  in
  let content = top_nav :: content in
  let body = Foundation.body ~title:"Mirage OS" ~headers:[] ~content ~trailers:[] () in
  Foundation.page ~body

let site =
  let fsite = Gen_site.flatten_site
      ~blog:(fun _ p -> List.assoc p Mirage_blog.blogs)
      ~wiki:(fun _ _ -> assert false)
      ~links:(fun _ _ -> assert false)
      ~file:(fun _ file -> read_file file)
      content
  in
  let h = Gen_site.make_pages ~config ~decorate fsite in
  Hashtbl.iter (fun n _ -> print_endline n) h ;
  let f x = try Some (Hashtbl.find h x) with Not_found -> None in
  f
