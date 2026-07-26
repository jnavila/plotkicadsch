(**
   Signatures for the whole library

   Copyright © Inria 2022

   @author Benoît Montagu <benoit.montagu@inria.fr>
*)

(** Module type of S-expressions *)
module type SEXP = sig
  type t = Atom of string | List of t list
end

module type S = sig
  type sexp
  (** The type of S-expressions *)

  type 'a decoder
  (** Type of decoders: a decoder of type ['a decoder] decodes a list of
   S-expressions into a value of type ['a]. A decoder contains a state
   of which S-expression is next available, and which ones should be
   processed later. *)

  val run_with_result: 'a decoder -> sexp -> ('a, sexp) result

  val run_list : 'a decoder -> sexp list -> 'a option
  (** [run_list d l] tries to decode the list of S-expression [l] into a
   structure value, using the decoder [d] *)

  val run : 'a decoder -> sexp -> 'a option
  (** [run d s] tries to decode the S-expression [s] into a structure
   value, using the decoder [d]. This is the same as [run_list d [s]] *)

  (** {1 Monadic combinators} *)

  val return : 'a -> 'a decoder
  (** [return v] is the constant decoder: it always returns the value
   [v]. It does not consume any part of the input. *)

  val const : 'a -> 'a decoder
  (** [const v] is the same as [return v] *)

  val error : 'a decoder
  (** [error] is the decoder that always fails *)

  val fail_default : 'a -> 'b decoder -> 'a decoder
  (** [fail_default v d] fails when [d] succeeds, and succeeds by
   returning [v] when [d] fails *)

  val fail : 'a decoder -> unit decoder
  (** [fail d] is the same as [fail_default () d] *)

  val try_ : 'a decoder -> 'a decoder
  (** [try_ d] succeeds iff [d] succeeds and returns the value
      produced by [d]. However, the state remains unchanged. *)

  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  (** [map f d] succeeds and returns [f x] when [d] succeeds and returns
   [x]. When [d] fails, [map f d] fails as well. *)

  val ( >>| ) : 'a decoder -> ('a -> 'b) -> 'b decoder
  (** [d >>| f] is the same as [map f d] *)

  val ( let+ ) : 'a decoder -> ('a -> 'b) -> 'b decoder
  (** [let+ x = d in t] is the same as [map (fun x -> t) d] *)

  val bind : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
  (** Monadic bind: [bind d f] first decodes using [d], and if [d]
   succeeds with a value [v], then decoding is performed using [f v].
   *)

  val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
  (** [d >>= f] is the same as [bind d f] *)

  val ( let* ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
  (** [let* x = d in t] is the same as [bind d (fun x -> t)] *)

  val seq : 'a decoder -> 'b decoder -> 'b decoder
  (** [seq d1 d2] is the decoder that first decodes with [d1], drops its
   result, and decodes using [d2]. Fails when [d1] fails, and when
   [d1] succeeds and [d2] fails. *)

  val ( >>> ) : 'a decoder -> 'b decoder -> 'b decoder
  (** [d1 >>> d2] is the same as [seq d1 d2] *)

  val drop : 'a decoder -> unit decoder
  (** [drop d] is the same as [map (fun _ -> return ()) d] *)

  val ( <<< ) : 'a decoder -> 'b decoder -> 'a decoder
  (** Same as [seq d1 d2], but keeps the result of [d1] and drops the
   result of [d2] *)

  val or_else : 'a decoder -> 'a decoder -> 'a decoder
  (** [or_else d1 d2] first decodes with [d1], and stops there if [d1]
   succeeds. If [d1] fails, then decoding with [d2] is performed. *)

  val ( |+> ) : 'a decoder -> 'a decoder -> 'a decoder
  (** [d1 |+> d2] is the same as [or_else d1 d2] *)

  val first : 'a decoder list -> 'a decoder
  (** [first [d1; ... ; dn]] selects the first decoder [di] that
   succeeds *)

  val or_else_delayed : 'a decoder -> (unit -> 'a decoder) -> 'a decoder
  (** [or_else_delayed d1 (fun () -> d2)] behaves the same as [or_else
   d1 d2], with the difference that the evaluation of the second
   operand is delayed. This is useful when defining recursive
   decoders, when [d2] refers to a recursive call. *)

  val ( |+>> ) : 'a decoder -> (unit -> 'a decoder) -> 'a decoder
  (** [d |+>> f] is the same as [or_else_delayed d f] *)

  (** {1 Basic decoders} *)

  val raw : sexp decoder
  (** [raw] accepts [Atom s] and [List l], and returns that
   S-expression. Consumes one element. *)

  val skip : unit decoder
  (** [skip] accepts [Atom s] and [List l], and returns [()]. Consumes
   one element. *)

  val skip_all : unit decoder
  (** [skip_all] accepts everything until no element is available
   anymore. Consomes 0 or more elements. *)

  val atom : string decoder
  (** [atom] accepts [Atom s], and returns [s]. Consumes one element. *)

  val no_more : unit decoder
  (** [no_more] returns [()] when there is no more S-expression
   available. Consumones nothing. *)

  val bool : bool decoder
  (** [bool] accepts [Atom "true"] by returning [true], and accepts
   [Atom "false"] by returning [false]. Consumes one element. *)

  val int : int decoder
  (** [int] accepts [Atom s] by returning the integer [n] denoted by the
   string [s]. Consumes one element. *)

  val float : float decoder
  (** [int] accepts [Atom s] by returning the float [f] denoted by the
   string [s]. Consumes one element. *)

  val string : ?escaped:bool -> string decoder
  (** [string ~escaped] accepts [Atom s] and returns [s] when
   [escaped=false], or the unescaped interpretation of [s] when
   [escaped=true]. Default: [escaped=false]. Consumes one element. *)

  val tag : string -> string decoder
  (** [tag name] accepts [Atom s] when [s = name], and returns [s].
   Consumes one element. *)

  (** {1 Combinators for compound data} *)

  (** {2 Basic combinators} *)

  val peek : 'a decoder -> 'a option decoder
  (** [peek d] returns [Some v] if [d] succeeds with [v], or returns
   [None] otherwise, and restores the state as it was before calling
   [d]. Consumes nothing. *)

  val group : 'a decoder -> 'a decoder
  (** [group d] accepts [List l] when [d] accepts [l], and returns the
   result provided by [d]. Consumes one element. *)

  val field : string -> 'a decoder -> 'a decoder
  (** [field name d] recognizes [List [Atom s; e1; ... ; en]] where
    [s = name] and [d] recognizes [[e1; ...; en]]. Consumes one element. *)

  val variant : (string * 'a decoder) list -> 'a decoder
  (** [variant cases] accepts [List [Atom name; e1; ...; en]] such that
   [(name, d)] is found in the association list [cases], and [d]
   accepts [[e1; ...; en]]. Only the first match for [name] is tried:
   the other ones are ignored. *)

  val record : default:'a -> (string * ('a -> 'a) decoder) list -> 'a decoder
  (** [record ~default [(name1, d1); ...; (namen, dn)]] recognizes
    [List [List (Atom name'1 :: l'1); ...; List (Atom name'k :: l'k)]]
    where for every [name'i], there exists [j] such that [name'i = namej],
    and [l'i] is accepted by [dj]. In case of success, the output value is
    [fjn ( ... (fj1 default) ...)] where the [fjk] are the values produced
    by [djk].

    The [record] combinator is useful to define decoders for records,
    in which fields may be given in any order. For every field, each
    decoder in the list produces a function that updates the
    corresponding field of the record. Each field entry is processed in
    the order in which they appear. Each field is optional.

    Example:

    {[
      # type t = { fst: int; snd: bool };;
      type t = { fst : int; snd : bool; }

      # let d =
        record ~default:{ fst = 0; snd = false }
          [ "fst", (let+ i = int in fun t -> { t with fst = i })
          ; "snd", (let+ b = bool in fun t -> { t with snd = b })
          ];;
      val d: t decoder = <abstr>
    ]}

    The decoder [d] will produce the following results:

    {[
      # run d (List [List [Atom "fst"; Atom "42"]; List [Atom "snd"; Atom "true"]])
      - : t option = Some {fst = 42; snd = true}

      # run d (List [List [Atom "snd"; Atom "false"]; List [Atom "fst"; Atom "42"]]);;
      - : t option = Some {fst = 42; snd = false}

      # run d (List [List [Atom "snd"; Atom "true"]]);;
      - : t option = Some {fst = 0; snd = true}

      # run d (List [List [Atom "fst"; Atom "42"]]);;
      - : t option = Some {fst = 42; snd = false}

      # run d (List [List [Atom "fst"; Atom "42"]; List [Atom "fst"; Atom "43"]]);;
      - : t option = Some {fst = 43; snd = false}
    ]}
 *)

  val fields : default:'a -> (string * ('a -> 'a) decoder) list -> 'a decoder
  (** [fields] has the same behaviour as [record], with the difference
   that the enclosing [List [...]] is not expected. In other words,
   [record ~default fs = group (fields ~default fs)]. *)

  val record_advanced :
    default:'a ->
    (string * [< `Required | `Unique ] list * ('a -> 'a) decoder) list ->
    'a decoder
  (** Same as [record], but with the ability to specify whether fields
   are mandatory (i.e., they must appear at least once), or whether
   they are unique (i.e., they must appear at most once). Fields with
   an empty list of options may appear 0, 1 or more times. *)

  val fields_advanced :
    default:'a ->
    (string * [< `Required | `Unique ] list * ('a -> 'a) decoder) list ->
    'a decoder
  (** [fields_advanced] has the same behaviour as [record_advanced],
   with the difference that the enclosing [List [...]] is not
   expected. In other words,
    [record_advanced ~default fs = group (fields_advanced ~default fs)]. *)

  (** {2 Combinators for tuples} *)

  val pair : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  (** [pair d1 d2] decodes a pair: it first decodes using [d1], then
   using [d2] if [d1] succeeds. The decoded result is [(v1, v2)],
   where [v1] is returned by [d1] and [v2] is return by [d2]. *)

  val ( <*> ) : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  (** [d1 <*> d2] is the same as [pair d1 d2] *)

  val tuple2 : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  (** [tuple2 d1 d2] is the same as [pair d1 d2] *)

  val tuple3 : 'a decoder -> 'b decoder -> 'c decoder -> ('a * 'b * 'c) decoder
  (** Similar to [tuple2], but decodes tuples of length 3 *)

  val tuple4 :
    'a decoder ->
    'b decoder ->
    'c decoder ->
    'd decoder ->
    ('a * 'b * 'c * 'd) decoder
  (** Similar to [tuple2], but decodes tuples of length 4 *)

  val tuple5 :
    'a decoder ->
    'b decoder ->
    'c decoder ->
    'd decoder ->
    'e decoder ->
    ('a * 'b * 'c * 'd * 'e) decoder
  (** Similar to [tuple2], but decodes tuples of length 5 *)

  val tuple6 :
    'a decoder ->
    'b decoder ->
    'c decoder ->
    'd decoder ->
    'e decoder ->
    'f decoder ->
    ('a * 'b * 'c * 'd * 'e * 'f) decoder
  (** Similar to [tuple2], but decodes tuples of length 6 *)

  val tuple7 :
    'a decoder ->
    'b decoder ->
    'c decoder ->
    'd decoder ->
    'e decoder ->
    'f decoder ->
    'g decoder ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) decoder
  (** Similar to [tuple2], but decodes tuples of length 7 *)

  val tuple8 :
    'a decoder ->
    'b decoder ->
    'c decoder ->
    'd decoder ->
    'e decoder ->
    'f decoder ->
    'g decoder ->
    'h decoder ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) decoder
  (** Similar to [tuple2], but decodes tuples of length 8 *)

  (** {2 Combinators for options and lists} *)

  val option : 'a decoder -> 'a option decoder
  (** Decoder for the standard representation of options as
   S-expressions.

   [option d] decodes [List []] into [None], and [List [e]] into [Some v]
    where the decoder [d] decodes [e] into [v]. *)

  val list : 'a decoder -> 'a list decoder
  (** Decoder for the standard representation of lists as S-expressions.

    [list d] decodes [List [e1; ...; en]] into the list [[v1; ...; vn]]
    where the decoder [d] decodes every element [ei] into a value [vi]. *)

  val list1 : 'a decoder -> 'a list decoder
  (** Same as [list], but for non-empty lists *)

  (** {2 Combinators for optional decoding} *)

  val maybe : 'a decoder -> 'a option decoder
  (** [maybe d] produces [Some v] if [d] produces a value [v], and [None] if [d] fails *)

  val maybe_with_default : 'a -> 'a decoder -> 'a decoder
  (** [maybe_with_default default d] produces [v] if [d] produces a
   value [v], and returns the value [default] if [d] fails *)

  (** {2 Combinators for repetitions} *)

  val repeat_list : until:unit decoder -> 'a decoder -> 'a list decoder
  (** [repeat_list ~until d] recognizes [e1; ...; en; f] such that
    [until] recognizes [f] and such that [d] recognizes each [ei]. If
    [di] produces [vi], then [repeat_list ~until d] produces the list
    [v1; ... ; vn]. [d] can succeed 0 or more times. *)

  val repeat1_list : until:unit decoder -> 'a decoder -> 'a list decoder
  (** [repeat1_list ~until d] is similar to [repeat_list ~until d], but
   forces that [d] succeeds at least once *)

  val repeat_full_list : 'a decoder -> 'a list decoder
  (** [repeat_full_list d] is the same as [repeat_list ~until:no_more d] *)

  val repeat1_full_list : 'a decoder -> 'a list decoder
  (** [repeat1_full_list d] is the same as [repeat1_list ~until:no_more d] *)

  val repeat : until:unit decoder -> unit decoder -> unit decoder
  (** [repeat ~until d] is similar to [repeat_list ~until d], but
   returns [()] instead of a list *)

  val repeat1 : until:unit decoder -> unit decoder -> unit decoder
  (** [repeat1 ~until d] is similar to [repeat1_list ~until d], but
   returns [()] instead of a list *)

  val repeat_fold_left :
    until:unit decoder -> init:'a -> ('a -> 'a) decoder -> 'a decoder
  (** [repeat_fold_left ~until ~init d] recognizes [e1; ...; en; f] such
   that [until] recognizes [f] and such that [d] recognizes each [ei].
   If [di] produces [fi], then [repeat_list ~until d] produces the
   result of the expression [fn (... (f1 init) ...)]. [d] can succeed
   0 or more times. *)

  val repeat_fold_right :
    until:unit decoder -> init:'a -> ('a -> 'a) decoder -> 'a decoder
  (** [repeat_fold_right ~until ~init d] recognizes [e1; ...; en; f]
   such that [until] recognizes [f] and such that [d] recognizes each
   [ei]. If [di] produces [fi], then [repeat_list ~until d] produces
   the result of the expression [f1 (... (fn init) ...)]. [d] can
   succeed 0 or more times. *)

  (** {2 Higher-order combinators} *)

  val filter : ('a -> bool) -> 'a decoder -> 'a decoder
  (** [filter p d] succeeds and returns [v] when [d] returns [v] and [p
   v = true]. Otherwise, [filter p d] fails. *)

  val fix : ((unit -> 'a decoder) -> 'a decoder) -> 'a decoder
  (** Fixpoint combinator: [fix f] is the decoder [d] such that
    [d = f (fun () -> d)] *)
end

module type INTF = sig
  module type SEXP = SEXP
  module type S = S

  (** Functor that is parameterized over the definition of
   S-expressions. This is useful, as several libraries in the OCaml
   redefine the same type, which leads to equivalent but incompatible
   type definitions. *)
  module Make (X : SEXP) : S with type sexp := X.t

  include S with type sexp := Csexp.t
  (** The code of the library is specialized on the definition of
   S-expressions that is provided by the [Csexp] library. *)
end
