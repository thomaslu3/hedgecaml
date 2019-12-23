(** A [Difficulty] is a representation of a game's difficulty.

    This module represents the difficulties of a game.
*)

(** The type of difficulties. *)
type difficulty =
  | Easy
  | Medium
  | Hard
  | Sandbox


(** Raised when an empty string or sequence of spaces is encountered. *)
exception Empty

(** Raised when an invalid difficulty is encountered. *)
exception Error

(** [make_command s] is the difficulty made from string [s].
    Raises: Error if [s] is not a valid difficulty. *)
val make_difficulty : string -> difficulty

(** [parse_diff s] parses string [s] into a difficulty. 
    Raises: Empty if the string is empty or a sequence of spaces. *)
val parse_diff : string -> difficulty