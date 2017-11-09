(******************************************************************************)
(* Copyright (c) 2016 Romain Bardou                                           *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining      *)
(* a copy of this software and associated documentation files (the            *)
(* "Software"), to deal in the Software without restriction, including        *)
(* without limitation the rights to use, copy, modify, merge, publish,        *)
(* distribute, sublicense, and/or sell copies of the Software, and to         *)
(* permit persons to whom the Software is furnished to do so, subject to      *)
(* the following conditions:                                                  *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be             *)
(* included in all copies or substantial portions of the Software.            *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                      *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE     *)
(* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION     *)
(* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION      *)
(* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.            *)
(******************************************************************************)

type single
type group

let opt_iter o f =
  match o with
    | None -> ()
    | Some x -> f x

let opt_map o f =
  match o with
    | None -> None
    | Some x -> Some (f x)

let split_string sep str =
  let len = String.length str in
  let position = ref len in
  let end_of_sub = ref len in
  let result = ref [] in
  while !position > 0 do
    let pos_before = !position in
    decr position;
    if str.[!position] = sep then
      let sub = String.sub str pos_before (!end_of_sub - pos_before) in
      end_of_sub := !position;
      result := sub :: !result
  done;
  let sub = String.sub str 0 !end_of_sub in
  sub :: !result

module Debug =
struct
  let alert x = Dom_html.window##alert(Js.string x)
  let log x = Firebug.console##log(Js.string x)
  let debug x = Firebug.console##debug(Js.string x)
  let info x = Firebug.console##info(Js.string x)
  let warn x = Firebug.console##warn(Js.string x)
  let error x = Firebug.console##error(Js.string x)
  let group x = Firebug.console##group(Js.string x)
  let group_collapsed x = Firebug.console##groupCollapsed(Js.string x)
  let group_end () = Firebug.console##groupEnd()
  let time x = Firebug.console##time(Js.string x)
  let time_end x = Firebug.console##timeEnd(Js.string x)
end

module Cookie:
sig
  val set: name: string -> value: string -> ?expires: float ->
    ?domain: string -> ?path: string -> ?secure: bool -> unit -> unit
  val get: string -> string
end =
struct
  let make_string ~name ~value ?expires ?domain ?path ?(secure = false) () =
    let opt name = function None -> [] | Some x -> [ name^x ] in
    [
      [ name^"="^value ];
      opt "expires=" expires;
      opt "domain=" domain;
      opt "path=" path;
      if secure then [ "secure" ] else [];
    ]
    |> List.flatten
    |> String.concat ";"

  let set_no_cache ~name ~value ?expires ?domain ?path ?secure () =
    let expires =
      match expires with
        | None ->
            None
        | Some seconds ->
            let d = jsnew Js.date_now () in
            let _: float = d##setTime(d##getTime() +. seconds *. 1000.) in
            Some (Js.to_string d##toUTCString())
    in
    let s = make_string ~name ~value ?expires ?domain ?path ?secure () in
    Dom_html.document##cookie <- Js.string s

  let remove_no_cache name =
    set_no_cache ~name ~value: "" ~expires: (-1.) ()

  exception End_of_string

  let parse cookie =
    let len = String.length cookie in
    let index = ref 0 in
    let next () =
      if !index < len then
        let c = cookie.[!index] in
        incr index;
        c
      else
        raise End_of_string
    in
    let result = ref [] in
    let left_start = ref 0 in
    let left_end = ref 0 in
    let quote = ref [] in
    let get_left () =
      let left_start = !left_start in
      let left_end = !left_end in
      String.sub cookie left_start (left_end - left_start)
    in
    let get_right relative_right_end =
      let right_start = !left_end + 1 in
      let right_end = !index + relative_right_end in
      String.sub cookie right_start (right_end - right_start)
    in
    let get_right_quote () =
      String.concat "" (List.rev_map (String.make 1) !quote)
    in
    let record left right = result := (left, right) :: !result in
    let rec start_pair () =
      left_start := !index;
      match next () with
        | ' ' | ';' -> start_pair ()
        | '=' -> start_right ()
        | '"' -> left_end := !left_start; start_right_quote ()
        | _ -> continue_left ()
    and continue_left () =
      match next () with
        | exception End_of_string ->
            left_end := !index; record "" (get_left ());
        | ';' -> left_end := !index - 1; record "" (get_left ()); start_pair ()
        | '=' -> start_right ()
        | _ -> continue_left ()
    and start_right () =
      left_end := !index - 1;
      match next () with
        | ';' -> start_pair ()
        | '"' -> start_right_quote ()
        | _ -> continue_right ()
    and continue_right () =
      match next () with
        | exception End_of_string -> record (get_left ()) (get_right 0)
        | ';' -> record (get_left ()) (get_right (-1)); start_pair ()
        | _ -> continue_right ()
    and start_right_quote () =
      quote := [];
      continue_right_quote ()
    and continue_right_quote () =
      let record () = record (get_left ()) (get_right_quote ()) in
      match next () with
        | exception End_of_string -> record ()
        | '"' -> record (); start_pair ()
        | '\\' ->
            begin match next () with
              | exception End_of_string -> record ()
              | c -> quote := c :: !quote; continue_right_quote ()
            end
        | c -> quote := c :: !quote; continue_right_quote ()
    in
    begin
      try
        start_pair ()
      with End_of_string ->
        ()
    end;
    !result

  (* At the first [get] we parse cookies and store them in [cache].
     After that, when we set a cookie we update the cache.
     This means that when we use [get] again we don't have to re-parse. *)
  let cache = Hashtbl.create 8
  let initialized = ref false

  let initialize () =
    initialized := true;
    let list = parse (Js.to_string Dom_html.document##cookie) in
    let add (name, value) = Hashtbl.replace cache name value in
    List.iter add list

  let set ~name ~value ?expires ?domain ?path ?secure () =
    set_no_cache ~name ~value ?expires ?domain ?path ?secure ();
    if !initialized then
      Hashtbl.replace cache name value

  let get name =
    if not !initialized then initialize ();
    match Hashtbl.find cache name with
      | exception Not_found -> ""
      | value -> value
end

module type STORAGE =
sig
  val set: name: string -> value: string -> unit
  val get: string -> string option
  val remove: string -> unit
  val clear: unit -> unit
end

module type STORAGE_OBJECT =
sig
  val storage: unit -> Dom_html.storage Js.t Js.optdef
end

module Make_storage(X: STORAGE_OBJECT): STORAGE =
struct
  let set ~name ~value =
    Js.Optdef.iter (X.storage ()) @@ fun storage ->
    storage##setItem(Js.string name, Js.string value)

  let get name =
    Js.Optdef.case (X.storage ())
      (fun () -> None)
      (fun storage ->
         Js.Opt.case storage##getItem(Js.string name)
           (fun () -> None)
           (fun value -> Some (Js.to_string value)))

  let remove name =
    Js.Optdef.iter (X.storage ()) @@ fun storage ->
    storage##removeItem(Js.string name)

  let clear () =
    Js.Optdef.iter (X.storage ()) @@ fun storage ->
    storage##clear()
end

module Local_storage =
  Make_storage(struct let storage () = Dom_html.window##localStorage end)

module Session_storage =
  Make_storage(struct let storage () = Dom_html.window##sessionStorage end)

module URL_hash =
struct
  let get () =
    let hash = Dom_html.window##location##hash |> Js.to_string in
    if hash = "" then
      ""
    else if hash.[0] = '#' then
      String.sub hash 1 (String.length hash - 1)
    else
      hash

  (* [cache] is used to prevent [set] from triggering [on_change]. *)
  let cache = ref ""

  let set hash =
    if !cache <> hash then (
      cache := hash;
      Dom_html.window##location##hash <- Js.string hash
    )

  let on_change handler =
    let handler _ =
      let value = get () in
      if value <> !cache then (
        cache := value;
        handler ();
      );
      Js._true
    in
    Dom_html.window##onhashchange <- Dom.handler handler
end

exception Stop

type node =
  | Text of Dom.text Js.t
  | Img of Dom_html.imageElement Js.t
  | A of Dom_html.anchorElement Js.t
  | Button of Dom_html.buttonElement Js.t
  | P of Dom_html.paragraphElement Js.t
  | H1 of Dom_html.headingElement Js.t
  | H2 of Dom_html.headingElement Js.t
  | H3 of Dom_html.headingElement Js.t
  | H4 of Dom_html.headingElement Js.t
  | H5 of Dom_html.headingElement Js.t
  | H6 of Dom_html.headingElement Js.t
  | Ul of Dom_html.uListElement Js.t
  | Li of Dom_html.liElement Js.t
  | Table of Dom_html.tableElement Js.t
  | Thead of Dom_html.tableSectionElement Js.t
  | Tbody of Dom_html.tableSectionElement Js.t
  | Tfoot of Dom_html.tableSectionElement Js.t
  | Tr of Dom_html.tableRowElement Js.t
  | Th of Dom_html.tableCellElement Js.t
  | Td of Dom_html.tableCellElement Js.t
  | Div of Dom_html.divElement Js.t
  | Span of Dom_html.element Js.t
  | Form of Dom_html.formElement Js.t
  | Input of Dom_html.inputElement Js.t

let as_node = function
  | Text x -> (x :> Dom.node Js.t)
  | Img x -> (x :> Dom.node Js.t)
  | A x -> (x :> Dom.node Js.t)
  | Button x -> (x :> Dom.node Js.t)
  | P x -> (x :> Dom.node Js.t)
  | H1 x -> (x :> Dom.node Js.t)
  | H2 x -> (x :> Dom.node Js.t)
  | H3 x -> (x :> Dom.node Js.t)
  | H4 x -> (x :> Dom.node Js.t)
  | H5 x -> (x :> Dom.node Js.t)
  | H6 x -> (x :> Dom.node Js.t)
  | Ul x -> (x :> Dom.node Js.t)
  | Li x -> (x :> Dom.node Js.t)
  | Table x -> (x :> Dom.node Js.t)
  | Thead x -> (x :> Dom.node Js.t)
  | Tbody x -> (x :> Dom.node Js.t)
  | Tfoot x -> (x :> Dom.node Js.t)
  | Tr x -> (x :> Dom.node Js.t)
  | Th x -> (x :> Dom.node Js.t)
  | Td x -> (x :> Dom.node Js.t)
  | Div x -> (x :> Dom.node Js.t)
  | Span x -> (x :> Dom.node Js.t)
  | Form x -> (x :> Dom.node Js.t)
  | Input x -> (x :> Dom.node Js.t)

(* Each dynamic start with [up_to_date] set to [true].
   Properties on which it depends have an observer which sets
   it to [false]. If it was [true], the dynamic is added
   to the [dynamics_to_be_rebuilt] list.
   The list is emptied before dynamics are rebuilt. *)
type dynamic =
  {
    mutable up_to_date: bool;
    mutable node: node;
    rebuild: unit -> node;
  }

let dynamics_to_be_rebuilt = ref []

let rebuild_dynamics () =
  let old_focus: Dom_html.inputElement Js.t Js.opt =
    Js.Unsafe.get Dom_html.document "activeElement"
  in
  let dynamics = !dynamics_to_be_rebuilt in
  dynamics_to_be_rebuilt := [];
  let rebuild dynamic =
    let old_node = as_node dynamic.node in
    let new_node = dynamic.rebuild () in
    dynamic.up_to_date <- true;
    dynamic.node <- new_node;
    Js.Opt.iter (old_node##parentNode) @@ fun parent ->
    let _: Dom.node Js.t = parent##replaceChild(as_node new_node, old_node) in
    ()
  in
  List.iter rebuild dynamics;
  Js.Opt.iter old_focus (fun old_focus -> old_focus##focus())

module Base64 =
struct
  let pad = '='

  let table = [|
    'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H';
    'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P';
    'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X';
    'Y'; 'Z'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f';
    'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n';
    'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v';
    'w'; 'x'; 'y'; 'z'; '0'; '1'; '2'; '3';
    '4'; '5'; '6'; '7'; '8'; '9'; '-'; '_';
    pad;
  |]

  let encode ?(pad = false) str =
    let len = String.length str in
    let triplets = len / 3 + (if len mod 3 > 0 then 1 else 0) in
    let removed_pad =
      match len mod 3 with
        | 1 -> 2
        | 2 -> 1
        | _ -> 0
    in
    String.init (triplets * 4 - removed_pad) @@ fun i ->
    let o = i / 4 * 3 in
    let code =
      match i mod 4 with
        | 0 ->
            let x = str.[o] in
            Char.code x lsr 2
        | 1 ->
            let x = str.[o] in
            let y = if o + 1 >= len then '\000' else str.[o + 1] in
            ((Char.code x land 3) lsl 4)
            lor (Char.code y lsr 4)
        | 2 when o + 1 >= len -> 64
        | 2 ->
            let x = if o + 1 >= len then '\000' else str.[o + 1] in
            let y = if o + 2 >= len then '\000' else str.[o + 2] in
            ((Char.code x land 15) lsl 2)
            lor (Char.code y lsr 6)
        | 3 when o + 2 >= len -> 64
        | 3 ->
            let x = str.[o + 2] in
            Char.code x land 63
        | _ ->
            assert false
    in
    table.(code)

  exception Invalid_character

  let invert = function
    | 'A' -> 00 | 'B' -> 01 | 'C' -> 02 | 'D' -> 03
    | 'E' -> 04 | 'F' -> 05 | 'G' -> 06 | 'H' -> 07
    | 'I' -> 08 | 'J' -> 09 | 'K' -> 10 | 'L' -> 11
    | 'M' -> 12 | 'N' -> 13 | 'O' -> 14 | 'P' -> 15
    | 'Q' -> 16 | 'R' -> 17 | 'S' -> 18 | 'T' -> 19
    | 'U' -> 20 | 'V' -> 21 | 'W' -> 22 | 'X' -> 23
    | 'Y' -> 24 | 'Z' -> 25 | 'a' -> 26 | 'b' -> 27
    | 'c' -> 28 | 'd' -> 29 | 'e' -> 30 | 'f' -> 31
    | 'g' -> 32 | 'h' -> 33 | 'i' -> 34 | 'j' -> 35
    | 'k' -> 36 | 'l' -> 37 | 'm' -> 38 | 'n' -> 39
    | 'o' -> 40 | 'p' -> 41 | 'q' -> 42 | 'r' -> 43
    | 's' -> 44 | 't' -> 45 | 'u' -> 46 | 'v' -> 47
    | 'w' -> 48 | 'x' -> 49 | 'y' -> 50 | 'z' -> 51
    | '0' -> 52 | '1' -> 53 | '2' -> 54 | '3' -> 55
    | '4' -> 56 | '5' -> 57 | '6' -> 58 | '7' -> 59
    | '8' -> 60 | '9' -> 61 | '-' -> 62 | '_' -> 63
    | _ -> raise Invalid_character

  let decode str =
    let len = String.length str in
    let pad_count =
      if len = 0 then 0
      else if len = 1 then
        if str.[0] = '=' then 1 else 0
      else
        (if str.[len - 1] = '=' then 1 else 0) +
        (if str.[len - 2] = '=' then 1 else 0)
    in
    let quads = len / 4 + (if len mod 4 > 0 then 1 else 0) in
    let len = len - pad_count in
    let silent_pad_count =
      match len mod 4 with
        | 0 -> 0
        | 1 -> 2 (* Invalid Base64 encoding, assume missing byte is 0. *)
        | 2 -> 2
        | 3 -> 1
        | _ -> assert false
    in
    String.init (quads * 3 - pad_count - silent_pad_count) @@ fun i ->
    let o = i / 3 * 4 in
    let code =
      match i mod 3 with
        | 0 ->
            let x = invert str.[o] in
            let y = if o + 1 >= len then 0 else invert str.[o + 1] in
            (x lsl 2) lor (y lsr 4)
        | 1 ->
            let x = if o + 1 >= len then 0 else invert str.[o + 1] in
            let y = if o + 2 >= len then 0 else invert str.[o + 2] in
            ((x land 15) lsl 4) lor (y lsr 2)
        | 2 ->
            let x = if o + 2 >= len then 0 else invert str.[o + 2] in
            let y = if o + 3 >= len then 0 else invert str.[o + 3] in
            ((x land 3) lsl 6) lor y
        | _ ->
            assert false
    in
    Char.chr code

  let of_int i =
    (* We use a variable-length implementation which works for any integer
       size. First bit is 1 if negative. This means that there are two ways to
       represent zero: "A" and "g". This also means that, while prefixing
       a positive integer with "A" does not change its value, prefixing
       a negative integer with "A" will give an unexpected result.
       It also means that some numbers are prefixed by "A", like 32 ("Ag").
       Removing this "A" would turn the number into a negative integer. *)
    (* Maybe we could use two's complement instead? *)
    let positive = i >= 0 in
    let rec make acc i =
      if i < 32 then
        if positive then
          i :: acc
        else
          (i lor 32) :: acc
      else
        make ((i land 63) :: acc) (i lsr 6)
    in
    let digits = make [] (abs i) |> Array.of_list in
    String.init (Array.length digits) @@ fun i ->
    table.(digits.(i))

  (* May raise [Invalid_character]. *)
  let to_int str =
    let len = String.length str in
    let rec make acc pos =
      if pos >= len then
        acc
      else
        let digit = invert str.[pos] in
        let acc = (acc lsl 6) lor digit in
        make acc (pos + 1)
    in
    if len > 0 then
      let digit = invert str.[0] in
      if digit < 32 then
        make digit 1
      else
        - make (digit land 31) 1
    else
      0

  (* for i = 0 to max_int do *)
  (*   if i mod 10000 = 0 then *)
  (*     Printf.printf "%d -> %s (neg: %s) -> %d (neg: %d)\n%!" *)
  (*       i (of_int i) (of_int (-i)) *)
  (*       (to_int (of_int i)) (to_int (of_int (-i))) *)
  (*   else ( *)
  (*     if i <> (to_int (of_int i)) then *)
  (*       Printf.printf "ERROR: %d -> %s -> %d\n%!" *)
  (*         i (of_int i) (to_int (of_int i)); *)
  (*     let i = -i in *)
  (*     if i <> (to_int (of_int i)) then *)
  (*       Printf.printf "ERROR: %d -> %s -> %d\n%!" *)
  (*         i (of_int i) (to_int (of_int i)); *)
  (*   ) *)
  (* done *)
end

exception Invalid_representation

module Json =
struct
  let output x = Json.output x
  let unsafe_input x =
    try
      Json.unsafe_input x
    with _ ->
      raise Invalid_representation
end

module Property =
struct
  type 'a typ =
    {
      to_base64: 'a -> string;
      of_base64: string -> 'a;
    }

  let custom to_base64 of_base64 =
    {
      to_base64;
      of_base64;
    }

  let unit =
    {
      to_base64 = (fun () -> "");
      of_base64 = (function _ -> ());
    }

  let base64_of_bool = function
    | true -> "1"
    | false -> "0"

  let bool_of_base64 = function
    | "1" -> true
    | "0" -> false
    | _ -> raise Invalid_representation

  let bool =
    {
      to_base64 = base64_of_bool;
      of_base64 = bool_of_base64;
    }

  let int_of_base64 str =
    try
      Base64.to_int str
    with Base64.Invalid_character ->
      raise Invalid_representation

  let int =
    {
      to_base64 = Base64.of_int;
      of_base64 = int_of_base64;
    }

  let base64_of_float i =
    (* TODO: use base64 (fixed-length) representation of the bits somehow? *)
    Base64.encode (string_of_float i)

  let float_of_base64 str =
    try
      Pervasives.float_of_string (Base64.decode str)
    with Failure _ | Base64.Invalid_character ->
      raise Invalid_representation

  let float =
    {
      to_base64 = base64_of_float;
      of_base64 = float_of_base64;
    }

  let base64_of_string = Base64.encode

  let string_of_base64 str =
    try
      Base64.decode str
    with Base64.Invalid_character ->
      raise Invalid_representation

  let string =
    {
      to_base64 = base64_of_string;
      of_base64 = string_of_base64;
    }

  let unsafe_json =
    {
      to_base64 = (fun x -> base64_of_string (Js.to_string (Json.output x)));
      of_base64 = (fun x -> Json.unsafe_input (Js.string (string_of_base64 x)));
    }

  type cookie =
    {
      name: string;
      expires: float option;
      domain: string option;
      path: string option;
      secure: bool;
    }

  type storage_location = Local | Session

  type storage_encoding = Base64 | Unsafe_JSON

  type storage =
    {
      name: string;
      location: storage_location;
      encoding: storage_encoding;
    }

  type save =
    | Volatile
    | Cookie of cookie
    | URL
    | Storage of storage

  let cookie ?expires ?domain ?path ?(secure = false) name =
    Cookie {
      name;
      expires;
      domain;
      path;
      secure;
    }

  let storage ?(session = false) ?(unsafe_json = false) name =
    Storage {
      name;
      location = if session then Session else Local;
      encoding = if unsafe_json then Unsafe_JSON else Base64;
    }

  type 'a node_attachment =
    {
      on_set: 'a -> unit;
      node: node;
    }

  type 'a single_attachment =
    {
      mutable attachment: 'a node_attachment option;
    }

  type 'a group_attachment =
    {
      group_name: string;
      attachments: ('a, 'a node_attachment) Hashtbl.t;
    }

  type ('a, _) kind =
    | Single: 'a single_attachment -> ('a, single) kind
    | Group: 'a group_attachment -> ('a, group) kind

  type ('a, 'b) t =
    {
      save: save;
      typ: 'a typ;
      default: 'a;
      mutable value: 'a;
      kind: ('a, 'b) kind;
      mutable dynamics: dynamic list;
    }

  type e = E: _ t -> e

  type properties =
    {
      mutable all: e list;
      mutable cookie_or_storage: e list;
      mutable url: e list;
    }

  let properties =
    {
      all = [];
      cookie_or_storage = [];
      url = [];
    }

  let create save typ default kind =
    let property =
      {
        save;
        typ;
        default;
        value = default;
        kind;
        dynamics = [];
      }
    in
    let e = E property in
    properties.all <- e :: properties.all;
    begin match save with
      | Volatile ->
          ()
      | Cookie _ | Storage _ ->
          properties.cookie_or_storage <- e :: properties.cookie_or_storage
      | URL ->
          properties.url <- e :: properties.url
    end;
    property

  let single save typ default =
    create save typ default (Single { attachment = None })

  let group group_name save typ default =
    create save typ default
      (Group { group_name; attachments = Hashtbl.create 8 })

  let volatile x =
    single Volatile (custom (fun _ -> assert false) (fun _ -> assert false)) x

  let url_hash_must_be_updated = ref false

  let want_to_save ~refresh_only property =
    match property.save with
      | Volatile ->
          ()
      | Cookie cookie ->
          let value = property.typ.to_base64 property.value in
          Cookie.set
            ~name: cookie.name
            ~value
            ?expires: cookie.expires
            ?domain: cookie.domain
            ?path: cookie.path
            ~secure: cookie.secure
            ()
      | Storage storage ->
          if not refresh_only then
            let set =
              match storage.location with
                | Local -> Local_storage.set
                | Session -> Session_storage.set
            in
            let value =
              match storage.encoding with
                | Base64 -> property.typ.to_base64 property.value
                | Unsafe_JSON -> Js.to_string (Json.output property.value)
            in
            set ~name: storage.name ~value
      | URL ->
          if not refresh_only then
            (* The property will be set after the event handler is done, so that
               all modified URL properties are saved at the same time. *)
            url_hash_must_be_updated := true

  (* Version of set which is called automatically and not by user code.
     The difference is that [set_and_update_dynamics] does not update the node,
     as this could trigger an event which would set the property again. *)
  let set_and_update_dynamics property value =
    property.value <- value;
    want_to_save ~refresh_only: false property;
    let update_dynamic dynamic =
      if dynamic.up_to_date then (
        dynamic.up_to_date <- false;
        dynamics_to_be_rebuilt := dynamic :: !dynamics_to_be_rebuilt
      )
    in
    List.iter update_dynamic property.dynamics

  let set (type a) (type b) (property: (a, b) t) (value: a) =
    set_and_update_dynamics property value;
    match property.kind with
      | Single { attachment = None } ->
          ()
      | Single { attachment = Some na } ->
          na.on_set value
      | Group { attachments } ->
          Hashtbl.iter (fun _ na -> na.on_set value) attachments

  let assign_value_from_base64 property str =
    match property.typ.of_base64 str with
      | exception Invalid_representation ->
          ()
      | value ->
          property.value <- value

  let set_from_base64 property str =
    match property.typ.of_base64 str with
      | exception Invalid_representation ->
          ()
      | value ->
          set property value

  let load_cookies_and_storage () =
    let load (E property) =
      match property.save with
        | Volatile ->
            ()
        | Cookie cookie ->
            let str = Cookie.get cookie.name in
            assign_value_from_base64 property str
        | Storage storage ->
            let name = storage.name in
            let value =
              match storage.location with
                | Local -> Local_storage.get name
                | Session -> Session_storage.get name
            in
            opt_iter value @@ fun value ->
            let value =
              match storage.encoding with
                | Base64 -> property.typ.of_base64 value
                | Unsafe_JSON ->
                    match Json.unsafe_input (Js.string value) with
                      | exception Invalid_representation ->
                          property.value
                      | value ->
                          value
            in
            property.value <- value
        | URL ->
            ()
    in
    List.iter load properties.cookie_or_storage

  let save_cookies_and_storage ~refresh_only () =
    let save (E property) = want_to_save ~refresh_only property in
    List.iter save properties.cookie_or_storage

  let get property =
    property.value

  let save_urls () =
    if !url_hash_must_be_updated then
      let encode (E property) = property.typ.to_base64 property.value in
      let hash =
        properties.url
        |> List.map encode
        |> String.concat "."
      in
      (* An empty hash causes scrolling to the top. *)
      if hash = "" then URL_hash.set " " else URL_hash.set hash

  let load_urls () =
    let rec decode properties subs =
      match properties, subs with
        | [], _
        | _, [] ->
            ()
        | E property :: other_properties, sub :: other_subs ->
            set_from_base64 property sub;
            decode other_properties other_subs
    in
    let hash = URL_hash.get () in
    (* Remove the space added by [save_urls]. *)
    let hash = if hash = " " then "" else hash in
    decode properties.url (split_string '.' hash)

  let all () =
    properties.all

  let iter f =
    List.iter f properties.all

  let reset property =
    set property property.default

  let reset_all () =
    iter (fun (E property) -> reset property)

  module Symbols =
  struct
    let (!!) = get
    let (<--) = set
  end
end

module Style =
struct
  type property =
    | Position
    | Left
    | Right
    | Top
    | Bottom

  type t = (property * string) list

  let set_property node (property, value) =
    let value = Js.string value in
    match property with
      | Position -> node##style##position <- value
      | Left -> node##style##left <- value
      | Right -> node##style##right <- value
      | Top -> node##style##top <- value
      | Bottom -> node##style##bottom <- value

  let set node style =
    match style with
      | None -> ()
      | Some style -> List.iter (set_property node) style
end

let run_after_event_triggers () =
  rebuild_dynamics ();
  Property.save_urls ()

module Mouse_event =
struct
  type t = Dom_html.mouseEvent Js.t
  let client_x (ev: t) = ev##clientX
  let client_y (ev: t) = ev##clientY
  let page_x (ev: t) = Js.Optdef.to_option ev##pageX
  let page_y (ev: t) = Js.Optdef.to_option ev##pageY
  let screen_x (ev: t) = ev##screenX
  let screen_y (ev: t) = ev##screenY
end

module Html =
struct
  type t = node

  (* Helpers *)

  let set_class node c =
    opt_iter c (fun x -> node##className <- Js.string x)

  let handler_arg f =
    Dom.handler @@ fun x ->
    let continue =
      match f x with
        | exception Stop ->
            Js._false
        | _ ->
            Js._true
    in
    run_after_event_triggers ();
    continue

  let handler f =
    handler_arg (fun _ -> f ())

  let set_on_click node on_click =
    opt_iter on_click @@ fun h ->
    node##onclick <- handler h

  let set_on_input node on_input =
    opt_iter on_input @@ fun h ->
    node##oninput <- handler h

  let set_on_change node on_change =
    opt_iter on_change @@ fun h ->
    node##onchange <- handler h

  let set_on_mouse_over node on_mouse_over =
    opt_iter on_mouse_over @@ fun h ->
    node##onmouseover <- handler_arg h

  let set_on_mouse_move node on_mouse_move =
    opt_iter on_mouse_move @@ fun h ->
    node##onmousemove <- handler_arg h

  let set_on_mouse_out node on_mouse_out =
    opt_iter on_mouse_out @@ fun h ->
    node##onmouseout <- handler h

  let set_on_submit (node: Dom_html.formElement Js.t) on_submit =
    opt_iter on_submit @@ fun h ->
    node##onsubmit <- handler h

  let append_children node children =
    let append_node child =
      let _: Dom.node Js.t = node##appendChild(as_node child) in
      ()
    in
    List.iter append_node children

  (* Node Constructors *)

  let text value =
    let node = Dom_html.document##createTextNode(Js.string value) in
    (* let set_text value = *)
    (*   text##replaceData(0, text##length, Js.string value) *)
    (* in *)
    Text node

  let empty =
    text ""

  let img ?c ?alt ?title src =
    let node = Dom_html.(createImg document) in
    node##src <- Js.string src;
    set_class node c;
    opt_iter alt (fun x -> node##alt <- Js.string x);
    opt_iter title (fun x -> node##title <- Js.string x);
    Img node

  let a ?c ?href ?on_click children =
    let node = Dom_html.(createA document) in
    append_children node children;
    set_class node c;
    opt_iter href (fun x -> node##href <- Js.string x);
    set_on_click node on_click;
    A node

  let button ?c ?on_click children =
    let node = Dom_html.(createButton document) in
    append_children node children;
    set_class node c;
    set_on_click node on_click;
    Button node

  let p ?c children =
    let node = Dom_html.(createP document) in
    append_children node children;
    set_class node c;
    P node

  let h1 ?c children =
    let node = Dom_html.(createH1 document) in
    append_children node children;
    set_class node c;
    H1 node

  let h2 ?c children =
    let node = Dom_html.(createH2 document) in
    append_children node children;
    set_class node c;
    H2 node

  let h3 ?c children =
    let node = Dom_html.(createH3 document) in
    append_children node children;
    set_class node c;
    H3 node

  let h4 ?c children =
    let node = Dom_html.(createH4 document) in
    append_children node children;
    set_class node c;
    H4 node

  let h5 ?c children =
    let node = Dom_html.(createH5 document) in
    append_children node children;
    set_class node c;
    H5 node

  let h6 ?c children =
    let node = Dom_html.(createH6 document) in
    append_children node children;
    set_class node c;
    H6 node

  let ul ?c children =
    let node = Dom_html.(createUl document) in
    append_children node children;
    set_class node c;
    Ul node

  let li ?c children =
    let node = Dom_html.(createLi document) in
    append_children node children;
    set_class node c;
    Li node

  let table ?c children =
    let node = Dom_html.(createTable document) in
    append_children node children;
    set_class node c;
    Table node

  let thead ?c children =
    let node = Dom_html.(createThead document) in
    append_children node children;
    set_class node c;
    Thead node

  let tbody ?c children =
    let node = Dom_html.(createTbody document) in
    append_children node children;
    set_class node c;
    Tbody node

  let tfoot ?c children =
    let node = Dom_html.(createTfoot document) in
    append_children node children;
    set_class node c;
    Tfoot node

  let tr ?c children =
    let node = Dom_html.(createTr document) in
    append_children node children;
    set_class node c;
    Tr node

  let th ?c children =
    let node = Dom_html.(createTh document) in
    append_children node children;
    set_class node c;
    Th node

  let td ?c children =
    let node = Dom_html.(createTd document) in
    append_children node children;
    set_class node c;
    Td node

  let div ?c ?style ?on_click ?on_mouse_over ?on_mouse_move ?on_mouse_out
      children =
    let node = Dom_html.(createDiv document) in
    append_children node children;
    set_class node c;
    set_on_click node on_click;
    set_on_mouse_over node on_mouse_over;
    set_on_mouse_move node on_mouse_move;
    set_on_mouse_out node on_mouse_out;
    Style.set node style;
    Div node

  let span ?c ?style ?on_click ?on_mouse_over ?on_mouse_move ?on_mouse_out
      children =
    let node = Dom_html.(createSpan document) in
    append_children node children;
    set_class node c;
    set_on_click node on_click;
    set_on_mouse_over node on_mouse_over;
    set_on_mouse_move node on_mouse_move;
    set_on_mouse_out node on_mouse_out;
    Style.set node style;
    Span node

  let form ?c ?on_submit children =
    let node = Dom_html.(createForm document) in
    append_children node children;
    set_class node c;
    set_on_submit node on_submit;
    Form node

  (* Shared code for all single inputs. *)
  let input _type ?c (property: ('a, single) Property.t)
      (on_set: _ -> 'a -> unit) =
    match property.kind with
      | Property.Single single ->
          match single.attachment with
            | Some { node = Input node }
              when Js.to_string node##_type = _type ->
                (* Reuse existing node. *)
                node
            | _ ->
                let node =
                  Dom_html.(createInput ~_type: (Js.string _type) document)
                in
                single.attachment <-
                  Some {
                    on_set = on_set node;
                    node = Input node;
                  };
                set_class node c;
                node

  type update_mode =
    | On_input
    | On_change

  let input_text_gen (type a) _type
      (a_of_string: string -> a option)
      (string_of_a: a -> string)
      ?c ?(mode = On_input) ?placeholder (property: (a, single) Property.t) =
    let node =
      input _type ?c property @@ fun node new_value ->
      node##value <- Js.string (string_of_a new_value)
    in
    node##value <- Js.string (string_of_a property.value);
    (
      match placeholder with
        | None ->
            ()
        | Some placeholder ->
            node##placeholder <- Js.string placeholder
    );
    let on_update () =
      opt_iter (a_of_string (Js.to_string node##value)) @@ fun value ->
      Property.set_and_update_dynamics property value
    in
    let set_update_event =
      match mode with
        | On_input -> set_on_input
        | On_change -> set_on_change
    in
    set_update_event node (Some on_update);
    Input node

  let id x = x
  let some x = Some x
  let int_option_of_string x =
    try
      Some (int_of_string x)
    with Failure _ ->
      None
  let float_option_of_string x =
    try
      Some (float_of_string x)
    with Failure _ ->
      None

  let input_text = input_text_gen "text" some id
  let input_int = input_text_gen "text" int_option_of_string string_of_int
  let input_float = input_text_gen "text" float_option_of_string string_of_float
  let input_password = input_text_gen "password" some id

  let input_checkbox ?c (property: (bool, single) Property.t) =
    let node =
      input "checkbox" ?c property @@ fun node new_value ->
      node##checked <- Js.bool new_value
    in
    node##checked <- Js.bool property.value;
    let on_click () =
      Property.set_and_update_dynamics property (Js.to_bool node##checked)
    in
    set_on_click node (Some on_click);
    Input node

  let input_radio ?c ?on_click (property: ('a, group) Property.t) (value: 'a) =
    let { Property.group_name; attachments } =
      match property.kind with | Property.Group kind -> kind
    in
    (* Try to reuse existing node, or create a new one. *)
    let node =
      let create () =
        let _type = Js.string "radio" in
        let name = Js.string group_name in
        Dom_html.(createInput ~_type ~name document)
      in
      match Hashtbl.find attachments value with
        | exception Not_found ->
            create ()
        | { node = Input node }
          when Js.to_string node##_type = "radio"
            && Js.to_string node##name = group_name ->
            (* Reuse existing node. *)
            node
        | _ ->
            create ()
    in
    (* Attach the node to the property, or update the attachment. *)
    let on_set new_value =
      (* No need to set to false if new_value <> value: the radio which is
         set to true will cause other radios to be set to false
         automatically. *)
      if new_value = value then
        node##checked <- Js.bool true
    in
    Hashtbl.replace attachments value {
      on_set;
      node = Input node;
    };
    (* Set the node attributes. *)
    set_class node c;
    node##checked <- Js.bool (property.value = value);
    let on_click () =
      (match on_click with None -> () | Some on_click -> on_click ());
      Property.set_and_update_dynamics property value
    in
    set_on_click node (Some on_click);
    Input node

  let dynamic ?deps rebuild =
    let deps =
      match deps with
        | None -> Property.all ()
        | Some deps -> deps
    in
    let node = rebuild () in
    let dynamic =
      {
        up_to_date = true;
        node;
        rebuild;
      }
    in
    let add_to_property (Property.E property) =
      property.dynamics <- dynamic :: property.dynamics
    in
    List.iter add_to_property deps;
    node
end

let run ?focus create_html =
  let on_load _ =
    Property.load_cookies_and_storage ();
    Property.save_cookies_and_storage ~refresh_only: true ();
    Property.load_urls ();
    let on_hash_change () =
      Property.load_urls ();
      rebuild_dynamics ()
    in
    URL_hash.on_change on_hash_change;
    let html = create_html () |> as_node in
    let body =
      let elements =
        Dom_html.window##document##getElementsByTagName(Js.string "body")
      in
      Js.Opt.get elements##item(0) (fun () -> failwith "no <body>")
    in
    let _: Dom.node Js.t = body##appendChild(html) in
    let focus_attached (property: _ Property.t) =
      let Property.Single { attachment } = property.kind in
      opt_iter attachment @@ fun { node } ->
      match node with
        | Input input ->
            input##focus()
        | _ ->
            ()
    in
    opt_iter focus focus_attached;
    Js._true
  in
  Dom_html.window##onload <- Dom.handler on_load

let delay duration on_timeout =
  let on_timeout () =
    on_timeout ();
    run_after_event_triggers ()
  in
  let _: Dom_html.timeout_id =
    Dom_html.window##setTimeout(Js.wrap_callback on_timeout, duration)
  in
  ()

let now () =
  (jsnew Js.date_now ())##getTime()

type http_response =
  {
    code: int;
    content: string;
  }

let http_request ?(verb = "GET") ~url ?content_type ?content f =
  let request = XmlHttpRequest.create () in
  request##_open(Js.string verb, Js.string url, Js._true);
  (
    match content_type with
      | None ->
          ()
      | Some content_type ->
          request##setRequestHeader
            (Js.string "Content-type", Js.string content_type);
  );
  (
    match content with
      | None ->
          request##send(Js.null)
      | Some content ->
          request##send(Js.some (Js.string content))
  );
  let handler =
    Html.handler @@ fun () ->
    f {
      code = request##status;
      content = Js.to_string request##responseText;
    }
  in
  request##onload <- handler
