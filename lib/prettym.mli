type ppf
(** Abstract data corresponding to the encoder state
    and all its machinery. *)

type 'a t = ppf -> 'a -> ppf
(** The type for formatters of values of type ['a]. *)

type ('ty, 'v) order

val keval_order : (ppf -> 'v) -> ppf -> ('ty, 'v) order -> 'ty

val break : indent:int -> len:int -> ('v, 'v) order
(** [break ~indent ~len] tries to add [len] spaces into the current line. If it fails - if we overflow
    the current line - we insert a [new_line] and fill the beginning of the new line with [indent]
    space  characters. *)

val fws : ('v, 'v) order
(** [fws] is a specialization of {!break} with [indent = 1] and [spaces = 1]. This is the widely
    used token to express an opportunity to break the line and be conform to [RFC 822] and the
    {i folding-whitespace} token. *)

val spaces : int -> ('v, 'v) order
val cut : ('v, 'v) order
val const : 'a t -> 'a -> ('v, 'v) order
val atom : 'a t -> ('a -> 'v, 'v) order
val a : ('a t -> 'a -> 'v, 'v) order
val ( !! ) : 'a t -> ('a -> 'v, 'v) order
val ( $ ) : 'a t -> 'a -> ('v, 'v) order
val new_line : ('v, 'v) order
val tbox : int -> ('v, 'v) order
val bbox : ('v, 'v) order
val box : ('v, 'v) order
val close : ('v, 'v) order
val using : ('b -> 'a) -> 'a t -> 'b t
val string : string t
val bytes : Bytes.t t
val bigstring : Bigstringaf.t t
val breakable : string t
val char : char t
val list : sep:'x t * 'x -> 'v t -> 'v list t
val option : 'a t -> 'a option t

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

val concat : ('a, 'b) fmt -> ('b, 'c) fmt -> ('a, 'c) fmt
val keval : (ppf -> 'v) -> ppf -> ('ty, 'v) fmt -> 'ty
val eval : ppf -> ('ty, ppf) fmt -> 'ty

module Buffer : sig
  type t = Bigstring of Bigstringaf.t | String of string | Bytes of bytes

  val weight : t -> int
  (** Weight of {!t}. *)

  val sub : t -> int -> int -> t
  (** [sub t off len] does a sub operation of {!t} of [len] bytes starting at
     [off]. *)
end

module IOVec : sig
  type t = { buffer : Buffer.t; off : int; len : int }
  (** Type of IOVec. *)

  val weight : t -> int
  (** Weight of {!t}. *)

  val length : t -> int
  (** Length (in bytes) of {!t}. *)

  val lengthv : t list -> int
  (** Length (in bytes) of a list of {!t}. *)

  val shift : t -> int -> t
  (** [shift t n] shifts [n] bytes on [t]. *)

  val split : t -> int -> t * t
  (** [split t off] splits [t] at [off] point. *)

  val merge : t -> t -> t option
  (** [merge a b] tries to merge [a] and [b] into a new {!t}. *)
end

val io_buffer_size : int

val create :
  ?margin:int -> ?new_line:string -> emitter:(IOVec.t list -> int) -> int -> ppf

val is_empty : ppf -> bool
val flush : ppf -> ppf
val kflush : (ppf -> 'v) -> ppf -> 'v
val to_string : ?margin:int -> ?new_line:string -> 'a t -> 'a -> string

val to_stream :
  ?margin:int -> ?new_line:string -> 'a t -> 'a -> unit -> string option
