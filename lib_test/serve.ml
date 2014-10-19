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

open Printf
open Lwt

open Cohttp
open Cohttp_lwt_unix

let make_server () =
  let callback conn_id req body =
    let path = Uri.path (Request.uri req) in
    let rec make_page path = match Mirage_org.site path with
      | Some (`Atom x) ->
          Server.respond_string ~status:`OK ~body:Syndic.(XML.to_string @@ Atom.to_xml x) ()
      | Some (`Html x) ->
          Server.respond_string ~status:`OK ~body:(Cowabloga.Html.doc_to_string x) ()
      | Some (`Redirect s) -> make_page s
      |_ ->
          let fname = Server.resolve_file ~docroot:"lib_test" ~uri:(Request.uri req) in
          Server.respond_file ~fname ()
    in make_page path
  in
  let conn_closed (_,conn_id) () =
    Printf.eprintf "conn %s closed\n%!" (Connection.to_string conn_id);
  in
  let config = { Server.callback; conn_closed } in
  let ctx = Cohttp_lwt_unix_net.init () in
  Server.create ~ctx ~mode:(`TCP (`Port 8081)) config

let _ = Lwt_unix.run (make_server ())
