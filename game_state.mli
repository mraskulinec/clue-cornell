(** This is the signature for the module representing the current state
    of the game. It keeps track of information such as the game's
    solution, the hands of the players, ?? should we combine this with
    player_state? *)

type s

val init_state : Clue.cards -> s

val current_player_id : s -> int (* TODO : make of type player *)

(** The final solution generated by our game *)
val solution : Clue.cards -> Clue.card list

(** The list of card hands each player has NOTE: replaced by draw_hands
    below *)

(* val user_hands : 'a list *)

(** The number of players for this game *)
val num_players : int ref

(** [start_game n f] launches the backend of the Clue game: * it sets up
    the murder loaction, weapon, and murderer; * the board, the start
    room, and assigns each player a hand of cards. * REQUIRES: f is a
    valid JSON file game representation *)
val start_game : Yojson.Basic.t -> Clue.cards

(** [show_board ()] prints the Clue board. *)
val show_board : unit -> unit

(** [hands_for_players] prints out each player's hand *)
val hands_for_players : unit -> unit

(** [choose_char n] prints out prompts regarding characters and prompts
    each user to choose a character, for a total of n characters. *)
val choose_char : int -> unit

(** [get_num_players ()] prompts the user choose how many players are in
    the game. *)
val get_num_players : unit -> unit

(** [show_help ()] prints the rules of the game and other information to
    help the user. *)
val show_help : unit -> unit

(** [start_rules ()] starts the game by printing the rules to the user *)
val start_rules : unit -> unit

(** The game terminates and the solution is shown on the screen, as well
    as the winner. *)
val end_game : unit -> unit

(** Assigns each player a set of cards to deal with *)
val draw_hands :
  Yojson.Basic.t -> Clue.card list list -> int -> Clue.card list list

(** Returns the string representation of each user's hand *)
val print_hands : string -> Clue.card list list -> string

(** [user_turn ()] recursively takes in user input to update the game
    data. It stops being called when the user enters the keywords to
    giveup or accuse someone *)
val user_turn : unit -> unit