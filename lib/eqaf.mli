(** Eqaf - timing-safe comparison functions

    {1 Basics}

    In cryptography, a timing-attack is a side-channel attack in which the
   attacker attempts to compromise a cryptosystem by analyzing the time taken to
   execute cryptographic algorithms.

    In some cases, a process needs to compare two values (input value and
   expected password). An attacker can analyze time needed by
   {!String.compare}/{!String.equal} to calculate expected password.

    This side-channel attack is due implementation of
   {!String.compare}/{!String.equal} which leaves as soon as possible when it
   reachs a difference between [a] and [b]. By this way, time taken to compare
   two values differs if they are equal or not.

    Distribution provides a little example of this kind of attack where we
   construct step by step (byte per byte) expected value from time spended to
   execute [compare].

    Distribution wants to provide some functions which protect user against this
   kind of attack:

    {ul
    {- [equal] like {!String.equal}}
    {- [compare_be] like {!String.compare}}
    {- [compare_le] which is a {!String.compare} with a reverse operation on
   inputs}}

    These functions are tested to see how long they took to compare two equal
   values and two different values. See {i check} tool for more informations. *)

(** {2 Implementations} *)

val equal : string -> string -> bool
(** [equal a b] returns [true] if [a] and [b] are equals. [String.equal a b =
   equal a b] for any [a] and [b]. [equal] function avoid a {i timing-attack}
   where it spends the same time even if [a] and [b] are differents.*)

val compare_be : string -> string -> int
(** [compare_be a b] returns [0] if [a] is equal to [b], a negative integer if
   [a] if {i less} (lexicographically) than [b], and a positive integer if [a]
   is {i greater} (lexicographically) than [b].

    [compare_be a b] returns the same {i order} than [String.compare a b] for
   any [a] and [b] (but not necessary the same integer!). Order is defined as:

    {ul
    {- [compare_be a b < 0] means [a < b]}
    {- [compare_be a b > 0] means [a > b]}
    {- [compare_be a b = 0] means [a = b]}}

    About time, if [String.length a <> String.length b], [compare_be] does not
   look into [a] or [b] and no comparison in bytes will be done. *)

val compare_be_with_len : len:int -> string -> string -> int
(** [compare_be_with_len ~len a b] does {!compare_be a b} on [len] bytes.

    @raise [Invalid_argument] if [len] is upper than [String.length a] or
   [String.length b]. *)

val compare_le : string -> string -> int
(** [compare_le a b] is semantically [compare_be (rev a) (rev b)]. With [rev]
   reverses a string ([a = rev (rev a)]). *)

val compare_le_with_len : len:int -> string -> string -> int
(** [compare_le_with_len a b] is semantically [compare_be_with_len ~len (rev a)
   (rev b)]. With [rev] reverse a string ([a = rev (rev a)]).

    @raise [Invalid_argument] if [len] is upper than [String.length a] or
   [String.length b]. *)
