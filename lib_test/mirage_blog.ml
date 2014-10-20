open Mirage_people
open Mirage_entries
open Cowabloga
open Date
open Blog

let read ent =
  match Mirage_entries.read ent with
  | None -> Html.(div [pcdata ""])
  | Some b -> Html.Unsafe.data @@ Omd.to_html @@ Omd.of_string b


let entries   = Entry.([
    { date    = date (2010, 10, 11, 15, 0);
      authors = [anil];
      title   = "Self-hosting Mirage website";
      body    = read "/blog/welcome.md";
      file    = "self-hosting-mirage-website";
    };
    { date    = date (2011, 04, 11, 15, 0);
      authors = [anil];
      title   = "A Spring Wiki Cleaning";
      body    = read "/blog/spring-cleaning.md";
      file    = "spring-cleaning";
    };
    { date    = date (2011, 09, 29, 11, 10);
      authors = [anil];
      title   = "An Outing to CUFP 2011";
      body    = read "/blog/an-outing-to-cufp.md";
      file    = "an-outing-to-cufp";
    };
    { date    = date (2012, 02, 29, 11, 10);
      authors = [mort];
      title   = "Connected Cloud Control: OpenFlow in Mirage";
      body    = read "/blog/announcing-mirage-openflow.md";
      file    = "announcing-mirage-openflow";
    };
    { date    = date (2012, 9, 12, 0, 0);
      authors = [dave];
      title   = "Building a \"xenstore stub domain\" with Mirage";
      body    = read "/blog/xenstore-stub.md";
      file    = "xenstore-stub-domain";
    };
    { date    = date (2012, 10, 17, 17, 30);
      authors = [anil];
      title   = "Breaking up is easy to do (with OPAM)";
      body    = read "/blog/breaking-up-with-opam.md";
      file    = "breaking-up-is-easy-with-opam";
    };
    { date    = date (2013, 05, 20, 16, 20);
      authors = [anil];
      title   = "The road to a developer preview at OSCON 2013";
      body    = read "/blog/the-road-to-a-dev-release.md";
      file    = "the-road-to-a-dev-release";
    };
    { date    = date (2013, 07, 18, 11, 20);
      authors = [dave];
      title   = "Creating Xen block devices with Mirage";
      body    = read "/blog/xen-block-devices-with-mirage.md";
      file    = "xen-block-devices-with-mirage";
    };
  ])

let blogs = [
  "/blog/", {path = "/blog/" ; entries }
]
