open Cowabloga

let anil = Person.person
    ~name:"Anil Madhavapeddy"
    ~uri:"http://anil.recoil.org"
    ~email:"anil@recoil.org"

let thomas = Person.person
    ~name:"Thomas Gazagnaire"
    ~uri:"http://gazagnaire.org"
    ~email:"thomas@gazagnaire.org"

let raphael = Person.person
    ~name:"Raphael Proust"
    ~uri:"https://github.com/raphael-proust"
    ~email:"raphlalou@gmail.com"

let dave = Person.person
    ~name:"Dave Scott"
    ~uri:"http://dave.recoil.org/"
    ~email:"dave@recoil.org"

let balraj = Person.person
    ~name:"Balraj Singh"
    ~email:"balraj.singh@cl.cam.ac.uk"

let mort = Person.person
    ~name:"Richard Mortier"
    ~uri:"http://mort.io/"
    ~email:"mort@cantab.net"

let vb = Person.person
    ~name:"Vincent Bernardoff"
    ~uri:"https://github.com/vbmithr"
    ~email:"vb@luminar.eu.org"

let rights : Syndic.Atom.rights option =
  Some (Syndic.Atom.Text "All rights reserved by the author")
