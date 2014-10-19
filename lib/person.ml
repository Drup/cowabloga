open Syndic

type t = Atom.author = {
  name : string;
  uri : Uri.t option;
  email : string option;
}

let person ?uri ?email ~name =
  (* Option.map FFS $£A%ø§1*ù *)
  let uri = match uri with
      None -> None | Some x -> Some (Uri.of_string x)
  in
  { uri ; email ; name }

let to_html {uri ; name} =
  match uri with
    | Some uri -> Html.(a ~a:[a_href @@ Uri.to_string uri] [pcdata name])
    | None -> Html.pcdata name
