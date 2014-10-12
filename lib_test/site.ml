open Cowabloga
open Lwt
open Config

let read_file ent =
  match Mirage_entries.read ent with
  | None -> return @@ Html.(div [pcdata ""])
  | Some b -> return (Html.Unsafe.data @@ Omd.to_html @@ Omd.of_string b)


let rec config = {
  base_uri="http://localhost:8081";
  title = "The Mirage Blog";
  subtitle = Some "on building functional operating systems";
  rights = Mirage_people.rights;
  authors = [];
  read_file ;
}

and categories = [
  `Blog ("Blog",  "/blog");
  `Wiki ("Docs", "/docs");
  (* `Page ("API" , "/api"); *)
  (* `Page ("Community", "/community"); *)
  (* `Page ("About", "/about"); *)
]

let top_nav =
  Foundation.top_nav
    ~title:"Mirage OS"
    ~title_uri:"/"
    ~nav_links:[Foundation.Nav.(top ~align:`Left @@ of_content categories) ]


let index =
  let content = Foundation.Index.t ~top_nav in
  let body = Foundation.body ~title:"Mirage OS" ~headers:[] ~content ~trailers:[] () in
  Foundation.page ~body



let content = `Menu (`Html ("Mirage", fun () -> index), categories)




(* let posts = Lwt_unix.run (Blog.to_html config Mirage_blog.entries) *)

(* and blog_template blog post = *)
(*   let recent_posts = Blog.recent_posts config Mirage_blog.entries in *)
(*   let sidebar = Foundation.Sidebar.t ~title:"Recent Posts" ~content:recent_posts in *)
(*   let copyright = Html.pcdata "Anil Madhavapeddy" in *)
(*   let {Config. title; subtitle } = config in *)
(*   Foundation.Blog.t ~title ?subtitle ~sidebar ~posts ~copyright () *)


(* let blog = *)
(*   let headers = [] in *)
(*   let content = top_nav @ t in *)
(*   let body = Foundation.body ~title:"Mirage Musings" ~headers ~content ~trailers:[] () in *)
(*   Foundation.page ~body *)
