open Config

let rec flatten_site ~get_dir ~get_file (c : content) = match c with
  | (#page as p)
  | (`Blog _ | `Wiki _ | `Links _ as p) -> [p]
  | `Link _ -> []
  | `Menu (page, l) ->
      let index =
        match page with
          | `Cat _ -> []
          | (#page as p) -> (flatten_site ~get_dir ~get_file ) p
      in index @ List.flatten @@ List.map (flatten_site ~get_dir ~get_file) l

let rec get_all_feeds = function
  | Feed s -> [s]
  | Directory (_, l) ->
      List.flatten @@ List.map get_all_feeds l
  | File _ -> []


let agregate_feeds
    ?id ?updated ?subtitle ?title feeds =
  Syndic.Atom.aggregate ?id ?updated ?subtitle ?title
  @@ List.map (fun (uri, feed) -> (Some uri, feed)) (get_all_feeds feeds)
