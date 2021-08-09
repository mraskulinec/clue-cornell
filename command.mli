(** Parsing of player commands. *)

(** The type [room_phrase] represents the room phrase that can be part
    of a player's command.

    A [room_phrase] cannot be an empty list. *)
type room_phrase = string list

(** The type [guess_phrase] represents the room phrase that can be part
    of a player's command. The list is in the same order as the words in
    the original command. Examples:

    - If the player's command is
      ["accuse Martha Pollack, hall, Poison"], then the guess phrase is
      ["Martha"; "Pollack"; "hall"; "Poison"].

    A [guess_phrase] cannot be an empty list. *)
type guess_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a room phrase or guess phrase. *)
type command =
  | Go of room_phrase
  | Accuse of guess_phrase
  | Guess of guess_phrase
  | Next
  | Giveup
  | Help

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command] by breaking up
    the string as follows: The first word of [str] becomes a verb (Go,
    Accuse, or Giveup) The rest of the verb, if any, become the guess
    phrase or room phrase.

    Requires: [str] only contains alphanumeric and space characters.

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command does not have the correct format.
    A command is malformed if the verb is not "go", "accuse", or "quit",
    or if the verb is "go" but the room phrase is empty. *)
val parse : string -> command
