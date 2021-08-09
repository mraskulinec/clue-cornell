(** This is a signature for the backend of the game and to represent *
    the data stored in the JSON files and manipulating * that data. *)

(** The abstract type for the set of cards in a clue game *)
type cards

(** The abstract type for an individual clue card *)
type card

(** The string representation of a clue card *)
val string_of_card : card -> string

(** [from_json j] is the set of clue cards that j represents. REQUIRES:
    j is a valid json file *)
val from_json : Yojson.Basic.t -> cards

(** [random_solution c] generates a solution which consists in each of a
    weapon card, character card, and room card *)
val random_solution : cards -> card list

(**  *)
val record_to_matrix : cards -> card list list

(* [deck_maker nlst dlst] takes in a list of random numbers [nlst], and
   the current deck list [dlst] and returns the full deck list *)
val deck_maker : int list -> card list -> cards -> card list

(** [rand_hand c ] is each player's hand of cards in the game of Clue,
    in addition to the game solution *)
val rand_hand : Yojson.Basic.t -> int -> card list
