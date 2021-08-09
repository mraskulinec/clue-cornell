open Clue2
open Command
open Player_state
open Board
open ANSITerminal

(** the abstract type representing the state of the game *)
type s

(** [show_board ()] prints the Clue board. *)
val show_board : unit -> unit

(** [choose_char n num_players] prints out prompts regarding characters
    and prompts each user to choose a character, for a total of n
    characters. *)
val choose_char : int -> int -> int

(** [start_rules ()] starts the game by printing the rules to the user *)
val start_rules : unit -> int

(** [get_exits_of_r rm rms] returns the list of exits of room rm from
    the set of rooms rms *)
val get_exits_of_r : string -> Rooms.room list -> Rooms.exit list

(** [get_room_id_of_r s rms] is the integer id of a room identified by a
    string *)
val get_room_id_of_r : string -> Rooms.room list -> int

(** [check_exit r es] is true if the given room [r] is included in
    Rooms.exit list [es]. Otherwise, false. *)
val check_exit : string -> Rooms.exit list -> bool

(** [get_num_players ()] prompts the user choose how many players are in
    the game. *)
val get_num_players : unit -> int

(** [show_help ()] prints the rules of the game and other information to
    help the user. *)
val show_help : unit -> unit

(** The game terminates and the solution is shown on the screen, as well
    as the winner. *)
val end_game : unit -> unit

(** [user_turn ()] recursively takes in user input to update the game
    data. It stops being called when the user enters the keywords to
    giveup or accuse someone *)
val user_turn : int -> Player_state.p -> Player_state.p list -> unit

val get_user_index : string -> int
