type single
type group

module Debug:
sig
  val alert: string -> unit

  (** The following functions access the FireFox console. *)

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

(** Raise this from event handlers to cancel the default behavior.

    For instance, it can be raised from the [on_click] event of a button
    to prevent the [on_submit] event of the form to also be executed. *)
exception Stop

module Property:
sig
  type 'a typ

  val unit: unit typ
  val bool: bool typ
  val int: int typ
  val float: float typ
  val string: string typ

  type save =
    | Volatile
    | Cookie of string
    | URL

  type ('a, 'b) t
  type e = E: _ t -> e

  val single: save -> 'a typ -> 'a -> ('a, single) t
  val group: string -> save -> 'a typ -> 'a -> ('a, group) t
  val get: ('a, 'b) t -> 'a
  val set: ('a, 'b) t -> 'a -> unit

  val all: unit -> e list
  val reset: ('a, 'b) t -> unit
  val reset_all: unit -> unit

  module Symbols:
  sig
    val (!!): ('a, 'b) t -> 'a
    val (<--): ('a, 'b) t -> 'a -> unit
  end
end

module Html:
sig
  type t

  val text: string -> t
  val img: ?c: string -> ?alt: string -> ?title: string -> string -> t
  val a: ?c: string -> ?href: string -> ?on_click: (unit -> unit) -> t list -> t
  val button: ?c: string -> ?on_click: (unit -> unit) -> t list -> t
  val p: ?c: string -> t list -> t
  val div: ?c: string -> t list -> t
  val span: ?c: string -> t list -> t
  val form: ?c: string -> ?on_submit: (unit -> unit) -> t list -> t

  val input_text: ?c: string -> (string, single) Property.t -> t
  val input_password: ?c: string -> (string, single) Property.t -> t
  val input_checkbox: ?c: string -> (bool, single) Property.t -> t
  val input_radio: ?c: string -> ('a, group) Property.t -> 'a -> t

  val dynamic: ?deps: Property.e list -> (unit -> t) -> t
end

val run: ?focus: ('a, single) Property.t -> (unit -> Html.t) -> unit
