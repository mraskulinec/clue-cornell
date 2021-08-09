open Clue2
open Command
open Player_state
open Board
open ANSITerminal
open Rooms

type s = {
  current_player : Player_state.p;
  all_players : Player_state.p list;
}

(** [deck_list] is the list of Clue cards read in from the json file *)
let deck_list =
  Clue2.from_json_final (Yojson.Basic.from_file "game2.json")

(** [r] is the Rooms.r type extracted from the json file *)
let r = Rooms.from_json (Yojson.Basic.from_file "rooms.json")

(** [room_list rms] returns a room list given an r type *)
let room_list r = Rooms.get_rooms r

(** [solution] is the randomly generated triple of winning cards *)
let solution = Clue2.extract_solution deck_list

(** [print_solution ()] prints the gamee solution to the CLI *)
let print_solution () =
  match solution with
  | a, b, c ->
      "The solution was " ^ Clue2.card_name a ^ " with the "
      ^ Clue2.card_name c ^ " in the " ^ Clue2.card_name b

(** [user_cards] is the list of Clue cards without the solution *)
let user_cards = Clue2.remove_solution solution deck_list

(** [print_user_cards_h lst] prints the list of card names of a single
    card list where lst is the Clue2.card' list *)
let rec print_user_cards_h = function
  | [] ->
      ();
      print_endline "\npress enter to continue";
      read_line ()
  | h :: t ->
      print_endline (card_name h);
      print_user_cards_h t

(** [print_user_cards lst i] prints a specified users' hand of cards; i
    is the index of the user and lst is the matrix of user hands*)
let print_user_cards (lst : Clue2.card' list List.t) i =
  let lst' = List.nth lst i in
  print_user_cards_h lst'

(** [user_hands i] is the list of card lists divided between i players *)
let user_hands i = Clue2.randomize_game i i user_cards []

(** [current_player_id s] is the current player given the state s *)
let current_player_id st = st.current_player

(** [players s] is the list of all players given the current game state
    st *)
let players st = st.all_players

let show_board () =
  print_endline
    "    \
     |-----------------------------------------------------------------|\n\
    \    |                |            |          |         \
     |              |\n\
    \    |    COCKTAIL    |            |   HALL   |         |  \
     DUFFIELD    |\n\
    \    |     LOUNGE     |            |          |         \
     |              |\n\
    \    |-----------------            |          |          \
     --------------|\n\
    \    |                             \
     |----------|                        |\n\
    \    \
     |---------------|_                                                \
     |\n\
    \    |  JANSEN'S       \
     |                                               |\n\
    \    |   DINING        |                           \
     |-------------------| \n\
    \    |    ROOM         |         ~~~~~~~~~~~~~~    \
     |                   | \n\
    \    |  AT BETHE      _|          CORNELL CLUE     \
     |                   | \n\
    \    |---------------|               BOARD         |    LAW \
     LIBRARY    | \n\
    \    |                           ~~~~~~~~~~~~~~    \
     |                   | \n\
    \    |-----------------|                           \
     |                   | \n\
    \    |                 |                           \
     |                   |   \n\
    \    |                 |                           \
     |---|               |\n\
    \    |    TERRACE      |                               \
     |---------------|\n\
    \    |                 \
     |                                               |\n\
    \    \
     |-----------------|                                               \
     |\n\
    \    |                        |----------------------|      \
     |----------|\n\
    \    |-----------------|      |                      |      |  \
     HELEN   |   \n\
    \    |                 |_     |       LIBE SLOPE     |      |  \
     NEWMAN  |\n\
    \    |    BAILEY HALL    |    |                      |      \
     |          |\n\
    \    |                   |    |--|                |--|      \
     |          | \n\
    \    \
     |-----------------------------------------------------------------|"

let rec choose_char n num_players =
  match n with
  | 0 ->
      print_endline "All set?";
      num_players
  | i -> (
      print_endline
        ("Player "
        ^ string_of_int (num_players - i + 1)
        ^ " is "
        ^ s_of_pn
            (Player_state.get_player_name
               (num_players - i + 1)
               player_options));
      print_endline "\nHere is your hand of cards: \n ";
      print_user_cards
        (randomize_game num_players num_players user_cards [])
        (i - 1);
      (* Board.print_location_board "the hall"; *)
      print_endline
        "\n\
        \ \n\
        \ \n\
        \ \
         \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
      match read_line with
      | _ ->
          ();
          choose_char (n - 1) num_players)

let rec get_num_players () =
  print_endline "How many players are there?";
  let n = read_line () |> int_of_string in
  if n > 6 then (
    print_endline "That's too many players! Try again.";
    get_num_players ())
  else if n < 3 then (
    print_endline "Not enough players! Try again.";
    get_num_players ())
  else choose_char n n

let show_help () =
  show_board ();
  print_endline
    "\n\
    \ Here are the rules of the game:\n\n\
    \ 1. You may have 3-6 players\n\
    \ 2. Every player gets dealt a hand with a mixture of weapons, \
     people, and rooms.\n\
    \ 3. To enter a room, type 'go [room].' You may only directly \
     travel to the rooms adjacent to yours. \n\
    \ 4. To make an accusation, type 'accuse [person] [room] \
     [weapon]'. You will be told whether or not your guess was \
     correct. You must be in the room that corresponds with your room. \n\
    \ 5. When you think you've figured out who did it, where, and with \
     what, you can make a final accusation"

let start_rules () =
  show_help ();
  print_endline "Ready to play? Press space and then enter to continue.";
  (* not finished *)
  match read_line () with
  | " " -> get_num_players ()
  | _ -> failwith "nothing"

(* let rec press_space () = match read_line () with _ -> start_rules () *)

let end_game () =
  print_endline
    ("Good Game! " ^ print_solution () ^ ". Play again soon!")

(** [user_accuse s] ends the game and checks if the player's accusation
    is correct; if so, he wins the game. Otherwise, he loses. *)
let user_accuse s =
  end_game ();
  match solution with
  | a, b, c ->
      if
        List.nth s 1 = card_name a
        && List.nth s 2 = card_name b
        && List.nth s 3 = card_name c
      then print_endline "You Win!"
      else print_endline "You Lost!"

(** [user_guess s c] checks whether the user's guess at the solution
    matches any of the cards that players already have. It then informs
    them if that's the case *)
let rec user_guess s cards =
  match cards with
  | hd :: tl -> (
      match s with
      | [ a; b; c; d; e ] ->
          if card_name hd = a ^ " " ^ b then
            print_endline ("some user has " ^ a ^ " " ^ b)
          else if card_name hd = c then
            print_endline ("some user has " ^ c)
          else if card_name hd = d ^ " " ^ e then
            print_endline ("some user has " ^ d ^ " " ^ e)
          else user_guess s tl
      | _ -> failwith "unimplemented")
  | _ -> failwith "unimplemented"

(* [change_player n c] takes the number of players n and then
   current_player c and returns the next player *)
let change_player num_players current_player player_opt =
  match player_opt with
  | [ hd; hd1; hd2; hd3; hd4; hd5 ] ->
      if current_player = hd then hd1
      else if current_player = hd1 then hd2
      else if current_player = hd2 && num_players > 3 then hd3
      else if current_player = hd2 then hd
      else if current_player = hd3 && num_players > 4 then hd4
      else if current_player = hd3 then hd
      else if current_player = hd4 && num_players > 5 then hd5
      else hd
  | _ -> failwith "unimplemented"

(** [get_room i c] returns the corresponding string name of a given room
    id *)
let rec get_room room_id (cards : Clue2.card' list) =
  match cards with
  | hd :: tl ->
      if card_id hd = room_id then card_name hd else get_room room_id tl
  | _ -> failwith "unimplemented"

(** [get_user_index p] returns the integer index of each of the provided
    avatars *)
let rec get_user_index player_name =
  if player_name = "Martin Y. Tang" then 0
  else if player_name = "Goldwin Smith" then 1
  else if player_name = "Mann" then 2
  else if player_name = "Happy Dave" then 3
  else if player_name = "G.K. Chesterton" then 4
  else 5

let rec get_exits_of_r (rm : string) rooms =
  match rooms with
  | hd :: tl ->
      if room_name hd = rm then Rooms.get_exits hd
      else get_exits_of_r rm tl
  | [] -> failwith "not a valid exit of current room"

let rec get_room_id_of_r (rm : string) rooms =
  match rooms with
  | hd :: tl ->
      if room_name hd = rm then Rooms.get_room_id hd
      else get_room_id_of_r rm tl
  | [] -> failwith "not a valid room"

let rec check_exit (rf : string) (exits : Rooms.exit list) =
  match exits with
  | [] -> false
  | hd :: tl ->
      if exit_name_of_exit hd = rf then true else check_exit rf tl

(** [switch_room n ri rf] is the list of player states that results from
    a player moving from one room to another *)
let switch_room name (ri : string) (rf : string) =
  if check_exit rf (get_exits_of_r ri (room_list r)) then
    let p = make_p name (id_of_i (get_room_id_of_r ri (room_list r))) in
    Player_state.change_room p
      (id_of_i (get_room_id_of_r rf (room_list r)))
      (get_user_index (s_of_pn name))
      name
  else failwith "exit is not a valid exit"

let rec user_turn
    num_players
    (current_player : Player_state.p)
    player_opt =
  (* print user's cards * print board and location on it * roll die move
     to a room * end turn by guessing or accusing *)
  let name = name_of_p current_player in
  let room = room_of_p current_player in
  let player_num = get_user_index (s_of_pn name) in
  let room_name = get_room (i_of_id room) deck_list in
  print_endline
    "\n\
    \ \n\
    \ \n\
    \ \
     \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  print_endline ("it is " ^ s_of_pn name ^ "'s turn \n");
  Board.print_location_board room_name;
  print_endline "\nYour cards are: ";
  (* TODO: not sure what exactly print_user_cards does and if it will
     take player_num (starts at 0) also if it also prints the name of
     the user because we already have that *)
  print_user_cards (user_hands num_players) player_num;
  print_endline
    "Use keywords: give up, guess, next, accuse, go, or help to move\n\
    \     forward with the game!";
  match Command.parse (read_line ()) with
  | Giveup -> end_game ()
  | Guess s ->
      user_guess s user_cards;
      print_endline "type 'next' for the next user's turn ";
      user_turn num_players current_player player_opt
  | Next ->
      user_turn num_players
        (change_player num_players current_player player_opt)
        player_opt
  | Accuse s -> user_accuse s
  | Help ->
      show_help ();
      user_turn num_players current_player player_opt
  | Go s ->
      let word = List.nth s 0 ^ " " ^ List.nth s 1 in
      let new_player_opt = switch_room name room_name word in
      let new_current_player =
        List.nth new_player_opt (get_user_index (s_of_pn name))
      in
      user_turn num_players new_current_player new_player_opt
