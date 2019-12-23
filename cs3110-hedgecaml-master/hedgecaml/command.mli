(** A [Command] is a representation of a game's commands.

    This module represents the commands of a game.
*)

(** The type of phrases. *)
type phrase = string list

(** The type of commands. *)
type command = 
  | Help
  | Companies
  | Buy of phrase
  | Sell of phrase
  | Balance
  | Price of phrase
  | Portfolio
  | Wait of phrase
  | Save
  | Round
  | Quit

(** Raised when an empty command is encountered. *)
exception EmptyCommand

(** Raised when an invalid command is encountered. *)
exception ErrorParsing

(** [make_command s p] is the command made from string [s] and phrase [p].
    Raises: Error if [s] and [p] do not make a valid command. *)
val make_command : string -> phrase -> command

(** [parse s] parses string [s] into a command. 
    Raises: Empty if the string is empty or a sequence of spaces. *)
val parse : string -> command

(** [to_list p] is the list of strings in phrase [p]. *)
val to_list : phrase -> string list