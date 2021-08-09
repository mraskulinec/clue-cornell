(** [card'] is the record of any clue card, containing a name, id, and
    type. *)
type card'

(** [card_name c] is the name field of a card' type c *)
val card_name : card' -> string

val card_id : card' -> int

(** the string representation of a player *)
type player_name

(** the identifier for each room *)
type room_id

(** [s_of_pn p] is the string of a player's name *)
val s_of_pn : player_name -> string

(** [pn_of_s s] is the player_name of a given string *)
val pn_of_s : string -> player_name

(** [i_of_id r] is the int of a room_id *)
val i_of_id : room_id -> int

(** [id_of_i i] is the room_id of an int *)
val id_of_i : int -> room_id

(** [from_json1 j] is the card'list representation of cards from a json
    file *)
val from_json1 : Yojson.Basic.t -> Yojson.Basic.t list

(** [from_json j] takes in a single card represented as a json file and
    returns the card' type of that json representation *)
val from_json : Yojson.Basic.t -> card'

(** [from_json2 j] takes as input a json file and returns a card' list
    of all the cards represented in the json file *)
val from_json2 : Yojson.Basic.t list -> card' list

(** from_json_final j is the function we call that pulls everything
    together: it takes in a raw json file and returns a list of type
    card'. It depends on from_json1, from_json2, and indirectly depends
    on from_json *)
val from_json_final : Yojson.Basic.t -> card' list

(** [extract_solution l] is the triple of cards that will be the
    solution for the game of cluee *)
val extract_solution : 'a list -> 'a * 'a * 'a

(** [remove_solution s l] takes as input the return value of
    [extract_solution lst] and the original list of cards. It returns
    the list of cards with the solution removed, i.e. the deck of cards
    to be dealt to the players. *)
val remove_solution : card' * card' * card' -> card' list -> card' list

(** [randomize_game i l] returns a list of card' lists corresponding to
    the number of players. It will randomly divide the given cards
    between the number of players i. REQUIRES: lst is the card' list
    that does not include the solution set of cards *)
val randomize_game :
  int -> int -> card' list -> card' list List.t -> card' list List.t

val make_card : string -> int -> string -> card'
