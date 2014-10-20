open Site

type flat_content = [
  | `File of name * file * [ `Div] Html.elt
  | `Html of name * [`Div] Html.elt
  | `Blog of Blog.blog
  | `Wiki of Wiki.wiki
  | `Links of Links.links
]

type flat_site = flat_content * flat_content list

let flatten_site ~blog ~wiki ~links ~file (c : Site.content) : flat_site =
  let rec get_index = function `Menu ((#page as page),_) | page -> get_content page

  and get_content : _ -> flat_content = function
    | `File (n, f) -> `File (n, f, file n f)
    | (`Html _ as p) -> p
    | `Blog (name, directory) -> `Blog (blog name directory)
    | `Wiki (name, directory) -> `Wiki (wiki name directory)
    | `Links (name, file) -> `Links (links name file)
    | _ -> failwith "No content"

  and flatten = function
    | `Link _ -> []
    | `Menu ((#page as page), l) ->
        let index = get_content page
        in index :: (List.flatten @@ List.map flatten l)
    | c -> [get_content c]
  in (get_index c, flatten c)


let get_path : flat_content -> string = function
  | `File (_, path, _)
  | `Html (path, _ )
  | `Blog {Blog. path }
  | `Wiki {Wiki. path }
  | `Links {Links. path }
      -> path

let make_pages ~config ~decorate (c : flat_site) =
  let h = Hashtbl.create 17 in
  let add name html = Hashtbl.add h name @@ `Html (decorate html) in
  let add_feed name feed = Hashtbl.add h name @@ `Atom feed in
  let aux = function
    | `File (_, path, h) | `Html (path, h) -> add path (`Page h)
    | `Blog blog ->
        let open Blog in
        add blog.path (`BlogIndex blog) ;
        List.iter
          (fun e -> add (Blog.Entry.permalink blog e) (`BlogEntry (blog, e)))
          blog.entries ;
        add_feed (blog.path ^ "update.xml") @@ to_atom config blog
    | `Wiki wiki ->
        let open Wiki in
        List.iter
          (fun e -> add (wiki.path ^ e.title) (`WikiEntry e.body))
          wiki.entries ;
        add_feed (wiki.path ^ "update.xml") @@ to_atom config wiki
    | `Links links ->
        let open Links in
        add (links.path ^ ".html") (`Links links) ;
        add_feed (links.path ^ ".xml") @@ to_atom config links
  in
  List.iter aux @@ snd c ;
  Hashtbl.add h "/" (`Redirect (get_path @@ fst c)) ;
  h
