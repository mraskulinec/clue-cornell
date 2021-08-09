(** the abstract type of a room's exit *)
type exit

(** the abstract type of a room *)
type room

(** the abstract type representing the rooms in a game of clue *)
type r

(** the start rooms given an abstract type r *)
val start_room : r -> string

(** [exit_name r] is the string name of room/exit r *)
val exit_name : room -> string

(** [exit_name_of_exit e] is the string representation of an abstract
    type exit *)
val exit_name_of_exit : exit -> string

(** [get_exits r] is the list of exits from a room type r *)
val get_exits : room -> exit list

(** [make_exit n t] is the exit specified by name n and type t *)
val make_exit : string -> string -> exit

(** [make_room r id ex] is the room specified by name r, id, and exits
    ex *)
val make_room : string -> int -> exit list -> room

(** [get_rooms r] is the list of rooms of type r *)
val get_rooms : r -> room list

(** [get_room_id rm] is the integer id of room rm *)
val get_room_id : room -> int

(** [room_name rm] is the string name of room rm *)
val room_name : room -> string

(** [room_names r] is the string list of room names of type r *)
val room_names : r -> string list

(** [from_json t] is the rooms type r represented by a json file t *)
val from_json : Yojson.Basic.t -> r
