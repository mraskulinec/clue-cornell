open Yojson.Basic.Util
open Random

type name = string

type character = {
  name : name;
  id : int;
  desc : string;
}

type exit = {
  name : name;
  types : string;
}

type room = {
  name : name;
  id : int;
  exits : exit list;
}

type weapon = {
  name : string;
  id : int;
}

type solution = {
  scharacter : character;
  sroom : room;
  sweapon : weapon;
}

type card =
  | Char_card of character
  | Room_card of room
  | Weapon_card of weapon

(* type cards = { characters : card list; rooms : card list; weapons :
   card list; } *)

type cards = {
  characters : character list;
  rooms : room list;
  weapons : weapon list;
}

(* let rec to_card acc constructor (lst : 'a list) : card list = match
   lst with | [] -> acc | h :: t -> to_card (constructor h :: acc)
   constructor t *)

let rec to_card_char acc lst : card list =
  match lst with
  | [] -> acc
  | h :: t -> to_card_char (Char_card h :: acc) t

let rec to_card_room acc lst : card list =
  match lst with
  | [] -> acc
  | h :: t -> to_card_room (Room_card h :: acc) t

let rec to_card_weapon acc lst : card list =
  match lst with
  | [] -> acc
  | h :: t -> to_card_weapon (Weapon_card h :: acc) t

let rec record_to_matrix (record : cards) : card list list =
  match record with
  | { characters; rooms; weapons } ->
      [ to_card_char [] characters ]
      @ [ to_card_room [] rooms ]
      @ [ to_card_weapon [] weapons ]

(* card list:

   [ Char_card (data) ; Room_card (Data); Char_card (data); Weapon_card
   (weapon) ] *)

let string_of_card (c : card) : string =
  match c with
  | Char_card c' -> c'.name
  | Room_card r' -> r'.name
  | Weapon_card w' -> w'.name

let helper_exit json : exit =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    types = to_string (List.assoc "type" (to_assoc json));
  }

(* to_assoc makes it of type json (it'll make it a string json pair) *)
let rec helper_exit_list json : exit list =
  match json with
  | [] -> []
  | hd :: tl -> helper_exit hd :: helper_exit_list tl

let helper_room json : room =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    id = json |> to_assoc |> List.assoc "id" |> to_int;
    (* id = (List.assoc "id" (to_assoc json)); *)
    exits =
      json |> to_assoc |> List.assoc "exit" |> to_list
      |> helper_exit_list;
  }

let helper_character json : character =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    id = to_int (List.assoc "id" (to_assoc json));
    desc = to_string (List.assoc "desc" (to_assoc json));
  }

let helper_weapon json : weapon =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    id = to_int (List.assoc "id" (to_assoc json));
  }

let rec helper_room_list json : room list =
  match json with
  | [] -> []
  | hd :: tl -> helper_room hd :: helper_room_list tl

let rec helper_character_list json : character list =
  match json with
  | [] -> []
  | hd :: tl -> helper_character hd :: helper_character_list tl

let rec helper_weapon_list json : weapon list =
  match json with
  | [] -> []
  | hd :: tl -> helper_weapon hd :: helper_weapon_list tl

let from_json json : cards =
  {
    characters =
      json |> to_assoc |> List.assoc "characters" |> to_list
      |> helper_character_list;
    rooms =
      json |> to_assoc |> List.assoc "rooms" |> to_list
      |> helper_room_list;
    weapons =
      json |> to_assoc |> List.assoc "weapons" |> to_list
      |> helper_weapon_list;
  }

let rec get_char_element i (lst : character list) =
  match lst with
  | [] -> failwith "not there character"
  | hd :: tl -> if hd.id = i then hd else get_char_element i tl

let rec get_room_element i (lst : room list) =
  match lst with
  | [] -> failwith "not there room"
  | hd :: tl -> if hd.id = i then hd else get_room_element i tl

let rec get_weapon_element i (lst : weapon list) =
  match lst with
  | [] -> failwith "not there weapon"
  | hd :: tl -> if hd.id = i then hd else get_weapon_element i tl

let random_solution deck =
  let ch = Random.int 5 in
  let ro = Random.int 9 + 5 in
  let we = Random.int 7 + 15 in

  []
  @ [ Char_card (get_char_element ch deck.characters) ]
  @ [ Room_card (get_room_element ro deck.rooms) ]
  @ [ Weapon_card (get_weapon_element we deck.weapons) ]

(** [random_solution_alt c] is an alternate way of generating a solution
    by creating a record. *)
let random_solution_alt c =
  {
    scharacter = get_char_element (Random.int 5) c.characters;
    sroom = get_room_element (Random.int 9 + 5) c.rooms;
    sweapon = get_weapon_element (Random.int 7 + 15) c.weapons;
  }

let rec deck_maker num_list deck_list c =
  match num_list with
  | [] -> deck_list
  | hd :: tl ->
      if hd < 6 then
        deck_maker tl deck_list c
        @ [ Char_card (get_char_element hd c.characters) ]
      else if hd < 15 then
        deck_maker tl deck_list c
        @ [ Room_card (get_room_element hd c.rooms) ]
      else
        deck_maker tl deck_list c
        @ [ Weapon_card (get_weapon_element hd c.weapons) ]

let rec rand_hand_helper num_cards int_list : int list =
  if num_cards = 0 then int_list
  else Random.int 22 :: rand_hand_helper (num_cards - 1) int_list

let json_file j = from_json (Yojson.Basic.from_file j)

let rand_hand json (num_cards : int) =
  let num_list = rand_hand_helper num_cards [] in
  deck_maker num_list [] (from_json json)
