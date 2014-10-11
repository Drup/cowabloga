(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
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

let soi = string_of_int

type month = int

let short_string_of_month m =
  match m with
  | 1  -> "Jan" | 2  -> "Feb" | 3  -> "Mar"
  | 4  -> "Apr" | 5  -> "May" | 6  -> "Jun"
  | 7  -> "Jul" | 8  -> "Aug" | 9  -> "Sep"
  | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _  -> "???"

let long_string_of_month m =
  match m with
  | 1  -> "January"
  | 2  -> "February"
  | 3  -> "March"
  | 4  -> "April"
  | 5  -> "May"
  | 6  -> "June"
  | 7  -> "July"
  | 8  -> "August"
  | 9  -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _  -> "???"

let xml_of_month m = Html.pcdata @@ short_string_of_month m

type date = {
  month : month;
  day   : int;
  year  : int;
  hour  : int;
  min   : int;
}

let html_of_date d =
  let open Html in
  span [pcdata @@ soi d.day ;
        pcdata @@ long_string_of_month d.month ;
        pcdata @@ soi d.year]

let html_of_date d =
  let open Html in
  div ~a:[a_class ["date"]] [
    div ~a:[a_class ["month"]] [xml_of_month d.month] ;
    div ~a:[a_class ["day"]]   [pcdata @@ soi d.day] ;
    div ~a:[a_class ["year"]]  [pcdata @@ soi d.year] ;
    div ~a:[a_class ["hour"]]  [pcdata @@ soi d.hour] ;
    div ~a:[a_class ["min"]]   [pcdata @@ soi d.min ] ;
]

let date (year, month, day, hour, min) =
  { month; day; year; hour; min }

let day (year, month, day) =
  { month; day; year; hour=0; min=0 }

let atom_date d =
  CalendarLib.Calendar.make d.year d.month d.day d.hour d.min 0
