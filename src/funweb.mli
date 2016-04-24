(** Tools for client-side (Javascript) web development. *)

module Debug:
sig
  (** Debugging tools (logging functions). *)

  (** Display a message in a blocking window. *)
  val alert: string -> unit

  (** {2 Firefox Console} *)

  val log: string -> unit
  val debug: string -> unit
  val info: string -> unit
  val warn: string -> unit
  val error: string -> unit
  val group: string -> unit
  val group_collapsed: string -> unit
  val group_end: unit -> unit

  (** Start a timer. The string identifies the timer. *)
  val time: string -> unit

  (** Stop a timer given its identifier and display its duration. *)
  val time_end: string -> unit
end

module Base64:
sig
  (** Base64 encoding. *)

  (** Encode in URL-friendly Base64.

      Characters which are used are [A..Za..z0..9-_], and [=] for padding
      if [pad] is [true]. By default [pad] is [false], so no padding is used,
      which is more URL-friendly. *)
  val encode: ?pad: bool -> string -> string

  (** Invert of [encode]. Accepts both padded and non-padded inputs. *)
  val decode: string -> string
end

module Cookie:
sig
  (** Cookie management. *)

  (** Set or replace a cookie.
      [expires] is in seconds starting from the current time. *)
  val set: name: string -> value: string -> ?expires: float ->
    ?domain: string -> ?path: string -> ?secure: bool -> unit -> unit

  (** Get the value of a cookie given its name. *)
  val get: string -> string
end

(** Phantom type for properties which are attached to a single node. *)
type single

(** Phantom type for properties which are attached to multiple nodes. *)
type group

module Property:
sig
  (** Properties which can be attached to HTML nodes. *)

  (** {2 Types} *)

  (** Type descriptions for properties. *)
  type 'a typ

  val unit: unit typ
  val bool: bool typ
  val int: int typ
  val float: float typ
  val string: string typ

  (** {2 Save Methods} *)

  (** Information for the [Cookie] save method. *)
  type cookie =
    {
      name: string;
      expires: float option; (** In seconds from current time. *)
      domain: string option;
      path: string option;
      secure: bool;
    }

  (** Save methods.

      - [Volatile] properties are not saved.
      - [Cookie] properties are saved in cookies.
        Note that cookies are sent to the HTTP server.
      - [URL] properties are saved in the hash part of the URL, i.e. after
        the [#]. The user can thus bookmark them and restore previous values
        using the Back button. *)
  type save =
    | Volatile
    | Cookie of cookie
    | URL

  (** Convenient way to build a [Cookie { ... }] value. *)
  val cookie: ?expires: float -> ?domain: string -> ?path: string ->
    ?secure: bool -> string -> save

  (** {2 Properties} *)

  (** Properties are enriched references which can be attached to HTML input
      nodes, such as text fields.
      When the user modifies the input, the property is automatically set to
      the new value. When the programmer sets the property to a new value,
      the input node is updated immediately.
      Properties can be automatically read from / written in cookies or
      the URL hash and are thus also a convenient way to save user data.
      Properties do not have to be attached to an input. This can be useful
      to save data which does not appear in an input field. *)

  (** Properties containing ['a] values.
      The second parameter (['b]) is a phantom type which is either [single]
      or [group]. *)
  type ('a, 'b) t

  (** Existential embeddings of properties. *)
  type e = E: _ t -> e

  (** Create a [single] property.

      Single properties can be attached to only one HTML input at a time.

      Usage: [single save typ default] *)
  val single: save -> 'a typ -> 'a -> ('a, single) t

  (** Create a [group] property.

      Group properties are used with radio buttons. Each button is associated
      to one value of the property.

      Usage: [group name save typ default]

      The [name] is used to identify the radio button group. *)
  val group: string -> save -> 'a typ -> 'a -> ('a, group) t

  (** Get the current value of a property. *)
  val get: ('a, 'b) t -> 'a

  (** Set the value of a property and update its attached node, if any. *)
  val set: ('a, 'b) t -> 'a -> unit

  (** Get the list of all properties. *)
  val all: unit -> e list

  (** Set a property to its default value. *)
  val reset: ('a, 'b) t -> unit

  (** Set all properties to their default values. *)
  val reset_all: unit -> unit

  module Symbols:
  sig
    (** Convenient symbols for {!get} and {!set}. *)

    (** Same as {!get}. *)
    val (!!): ('a, 'b) t -> 'a

    (** Same as {!set}. *)
    val (<--): ('a, 'b) t -> 'a -> unit
  end
end

(** Raise this from event handlers to cancel the default behavior.

    For instance, it can be raised from the [on_click] event of a button
    to prevent the [on_submit] event of the form to also be executed. *)
exception Stop

module Html:
sig
  (** HTML tree constructors. *)

  (** HTML nodes. *)
  type t

  (** Make a text node with no [<tag>]. *)
  val text: string -> t

  (** Make an image node ([<img>]). *)
  val img: ?c: string -> ?alt: string -> ?title: string -> string -> t

  (** Make an anchor (i.e. link) node ([<a>]). *)
  val a: ?c: string -> ?href: string -> ?on_click: (unit -> unit) -> t list -> t

  (** Make a button node ([<button>]). *)
  val button: ?c: string -> ?on_click: (unit -> unit) -> t list -> t

  (** Make a paragraph node ([<p>]). *)
  val p: ?c: string -> t list -> t

  (** Make a block group node ([<div>]). *)
  val div: ?c: string -> t list -> t

  (** Make an inline group node ([<span>]). *)
  val span: ?c: string -> t list -> t

  (** Make a form group node ([<form>]). *)
  val form: ?c: string -> ?on_submit: (unit -> unit) -> t list -> t

  (** {2 Input Nodes} *)

  (** Make an input node ([<input>]) with the [text] type. *)
  val input_text: ?c: string -> (string, single) Property.t -> t

  (** Make an input node ([<input>]) with the [password] type. *)
  val input_password: ?c: string -> (string, single) Property.t -> t

  (** Make an input node ([<input>]) with the [checkbox] type. *)
  val input_checkbox: ?c: string -> (bool, single) Property.t -> t

  (** Make an input node ([<input>]) with the [radio] type. *)
  val input_radio: ?c: string -> ('a, group) Property.t -> 'a -> t

  (** {2 Rebuildable Nodes} *)

  (** Make a node which depends on the value of some properties.

      Usage: [dynamic ?deps make]

      Each time the value of one of the properties in [deps] changes,
      the node shall be rebuilt by calling the [make] argument.

      By default, [deps] is the set of all properties. *)
  val dynamic: ?deps: Property.e list -> (unit -> t) -> t
end

(** Run a program.

    Usage: [run ?focus make]

    Argument [make] is used to build the HTML tree.

    If [focus] is a property which is attached to a node, this node
    takes the initial focus. This means that if the user starts typing,
    this node will receive the inputs.

    You should follow these two rules:
    - all properties must have been defined before [run] is called;
    - HTML nodes must be created by [make], not before [run]. *)
val run: ?focus: ('a, single) Property.t -> (unit -> Html.t) -> unit
