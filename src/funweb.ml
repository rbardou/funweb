type single
type group

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
    | _ -> invalid_arg "Base64.invert"

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
    String.init (quads * 3 - pad_count) @@ fun i ->
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
end

exception Stop

type node =
  | Text of Dom.text Js.t
  | Img of Dom_html.imageElement Js.t
  | A of Dom_html.anchorElement Js.t
  | Button of Dom_html.buttonElement Js.t
  | P of Dom_html.paragraphElement Js.t
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
  | Div x -> (x :> Dom.node Js.t)
  | Span x -> (x :> Dom.node Js.t)
  | Form x -> (x :> Dom.node Js.t)
  | Input x -> (x :> Dom.node Js.t)

let opt_iter o f =
  match o with
    | None -> ()
    | Some x -> f x

let opt_map o f =
  match o with
    | None -> None
    | Some x -> Some (f x)

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

module Property =
struct
  type 'a typ =
    {
      to_string: 'a -> string;
      of_string: string -> 'a;
    }

  let unit =
    {
      to_string = (fun () -> "");
      of_string = (function _ -> ());
    }

  let bool =
    {
      to_string = string_of_bool;
      of_string = bool_of_string;
    }

  let int =
    {
      to_string = string_of_int;
      of_string = int_of_string;
    }

  let float =
    {
      to_string = string_of_float;
      of_string = float_of_string;
    }

  let id x = x
  let string =
    {
      to_string = id;
      of_string = id;
    }

  type cookie =
    {
      name: string;
      expires: float option;
      domain: string option;
      path: string option;
      secure: bool;
    }

  type save =
    | Volatile
    | Cookie of cookie
    | URL

  let cookie ?expires ?domain ?path ?(secure = false) name =
    Cookie {
      name;
      expires;
      domain;
      path;
      secure;
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

  let properties = ref []

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
    properties := E property :: !properties;
    property

  let single save typ default =
    create save typ default (Single { attachment = None })

  let group group_name save typ default =
    create save typ default
      (Group { group_name; attachments = Hashtbl.create 8 })

  let save property =
    match property.save with
      | Volatile ->
          ()
      | Cookie cookie ->
          let value = property.typ.to_string property.value |> Base64.encode in
          Cookie.set
            ~name: cookie.name
            ~value
            ?expires: cookie.expires
            ?domain: cookie.domain
            ?path: cookie.path
            ~secure: cookie.secure
            ()
      | URL ->
          ()

  let load property =
    match property.save with
      | Volatile ->
          ()
      | Cookie cookie ->
          let value = Cookie.get cookie.name |> Base64.decode in
          property.value <- property.typ.of_string value
      | URL ->
          ()

  let get property =
    property.value

  (* Version of set which is called automatically and not by user code.
     The difference is that [set_and_update_dynamics] does not update the node,
     as this could trigger an event which would set the property again. *)
  let set_and_update_dynamics property value =
    property.value <- value;
    save property;
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

  let all () =
    !properties

  let reset property =
    set property property.default

  let reset_all () =
    List.iter (fun (E property) -> reset property) (all ())

  let save_all () =
    List.iter (fun (E property) -> save property) (all ())

  let load_all () =
    List.iter (fun (E property) -> load property) (all ())

  module Symbols =
  struct
    let (!!) = get
    let (<--) = set
  end
end

module Html =
struct
  type t = node

  (* Helpers *)

  let set_class node c =
    opt_iter c (fun x -> node##className <- Js.string x)

  let handler f =
    Dom.handler @@ fun _ ->
    let continue =
      match f () with
        | exception Stop ->
            Js._false
        | _ ->
            Js._true
    in
    rebuild_dynamics ();
    continue

  let set_on_click node on_click =
    opt_iter on_click @@ fun h ->
    node##onclick <- handler h

  let set_on_input node on_input =
    opt_iter on_input @@ fun h ->
    node##oninput <- handler h

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

  let div ?c children =
    let node = Dom_html.(createDiv document) in
    append_children node children;
    set_class node c;
    Div node

  let span ?c children =
    let node = Dom_html.(createSpan document) in
    append_children node children;
    set_class node c;
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

  let input_text_or_password _type ?c (property: (string, single) Property.t) =
    let node =
      input _type ?c property @@ fun node new_value ->
      node##value <- Js.string new_value
    in
    node##value <- Js.string property.value;
    let on_input () =
      Property.set_and_update_dynamics property (Js.to_string node##value)
    in
    set_on_input node (Some on_input);
    Input node

  let input_text = input_text_or_password "text"
  let input_password = input_text_or_password "password"

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

  let input_radio ?c (property: ('a, group) Property.t) (value: 'a) =
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

let get_hash () =
  let hash = Dom_html.window##location##hash |> Js.to_string in
  if hash = "" then
    ""
  else if hash.[0] = '#' then
    String.sub hash 1 (String.length hash - 1)
  else
    hash

let set_hash hash =
  Dom_html.window##location##hash <- Js.string hash

let on_hash_change handler =
  let handler _ = handler (); Js._true in
  Dom_html.window##onhashchange <- Dom.handler handler

let run ?focus create_html =
  let on_load _ =
    Property.load_all ();
    Property.save_all (); (* Refresh cookie expiration dates. *)
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
