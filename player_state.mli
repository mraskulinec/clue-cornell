(** This is the signature representing the module of the player state of
    the game. This module keeps track of the current player, each
    player's visited rooms, and the order in which the players take
    turns *)
open Clue2

(** The abstract type of the current player *)
type p

(** The starting player of the game *)
val initial_player : p

(** [change_room (p: player_name) (rf : room_id)] is the player state p
    after a player moves to room [rf]. *)
val change_room :
  p -> Clue2.room_id -> int -> Clue2.player_name -> p list

(** [make_p pn id] is a player p given the name and room *)
val make_p : Clue2.player_name -> Clue2.room_id -> p

(** [player_options] is the set of player names that individual users
    may be assigned *)
val player_options : p list

(** the name of the player specified by the given index *)
val get_player_name : int -> p list -> player_name

(** [name_of_p p] is the player name given a type p *)
val name_of_p : p -> Clue2.player_name

(** [room_of_p] is the player room given a type p *)
val room_of_p : p -> Clue2.room_id
