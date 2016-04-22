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

  type save =
    | Volatile
    | Cookie of string
    | URL

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

  let get property =
    property.value

  (* Modify value and dynamics, but do not update attached nodes. *)
  let set_and_update_dynamics property value =
    property.value <- value;
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

  let reset_all property =
    List.iter (fun (E property) -> reset property) (all ())

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
