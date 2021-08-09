open Clue
open Command
open ANSITerminal

(* open Player open Rooms *)

(* make sure we haeve a ton of variables for different information
   pieces of game *)
type s = {
  current_player : int;
  (* TODO: change type int to be a type from Player module *)
  all_players : int list;
}

(* anything requiring user i/o goes in main.ml; anything that changes or
   manipulates the state goes into state.ml *)
(* store which card each user has; make a player module; make a player
   list for type t of state *)
(* have initialize game function that takes in number of players and
   splits up cards accordingly; after that, show each player which cards
   they have; taking in names? *)
(* write code in state; factor it out from main.ml so main is easier to
   maniulate pass in commands from main into other fxns and then into
   main again so that we communicate between users and functions *)

let init_state adv =
  let first_player = 0 in
  { current_player = first_player; all_players = [ first_player ] }

(* TODO: modify so that all players are included in list *)

let current_player_id st = st.current_player

let players st = st.all_players

(* let rec player_from_id lst id acc = match lst with | [] -> failwith
   "player doesn't exist" | h::t -> if h = id then *)

type result =
  | Legal of s
  | Illegal

(** [go_helper adv st ex] is the room_id of the next room as designated
    by [adv], [st], and [ex]. *)

(* let go_helper adv st ex = next_room adv st.current_room ex *)

(* let go ex adv st = try let new_room = go_helper adv st ex in Legal {
   current_room = new_room; visited = new_room :: visited st |>
   List.sort_uniq compare; } with UnknownExit ex -> Illegal *)

let solution = Clue.random_solution

let user_hands_var = ref []

let num_players = ref 0

(* TODO: make a structure to store various players' characters and their turns/information 
 * players will have to take note of which character they are in the game; we 
 * will tell them when it's that character's turn *)

(** [start_game f] launches the backend of the Clue game: * it sets up
    the murder loaction, weapon, and murderer; * the board, the start
    room, and assigns each player a hand of cards. * REQUIRES: f is a
    valid JSON file game representation *)
let start_game f = Clue.from_json f

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

(* TODO: Make sure names are not hardcoded in the future *)

let rec draw_hands json (hands : Clue.card list list) n =
  if n = 0 then hands
  else
    draw_hands json (Clue.rand_hand json !num_players :: hands) (n - 1)

let rec print_hand acc (lst : Clue.card list) : string =
  match lst with
  | [] -> acc
  | h :: t -> print_hand (acc ^ " \n" ^ Clue.string_of_card h) t

let rec print_hands acc (mat : Clue.card list list) : string =
  match mat with
  | [] -> acc
  | h :: t ->
      print_hands
        (acc ^ "\n Next Player's cards: \n " ^ print_hand "" h)
        t

(* let hands_for_players () = match read_line () with | _ -> let cards =
   "game.json" |> Yojson.Basic.from_file |> start_game |>
   Clue.record_to_matrix in print_endline (print_hands "Printing\n
   Hands:\n " (draw_hands ("game.json" |> Yojson.Basic.from_file) cards
   !num_players)) *)

let hands_for_players () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "    \
     |-----------------------------------------------------------------|\n\
    \    |                |            |          |         \
     |              |\n\
    \    |    COCKTAIL    |            |   HALL   |         |  \
     DUFFIELD    |\n\
    \    |     LOUNGE     |            |     Ϙ    |         \
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
     |-----------------------------------------------------------------|\n\
    \ Ϙ = You are here!\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\n\
    \ Player 1's cards: \n\n\
    \ Claire Cardie \n\n\
    \ Rope \n\n\
    \ Terrace \n\n\
    \ Bailey Hall\n\n\
    \ Michael Clarkson \n\n\
    \ Revolver \n\n\
    \ Duffield\n";
  (match read_line () with
  | _ -> print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    "    \
     |-----------------------------------------------------------------|\n\
    \    |                |            |          |         \
     |              |\n\
    \    |    COCKTAIL    |            |   HALL   |         |  \
     DUFFIELD    |\n\
    \    |     LOUNGE     |            |     փ    |         \
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
     |-----------------------------------------------------------------|\n\
    \ փ = You are here!\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\n\
    \ Player 2's cards: \n\n\
    \ Poison \n\n\
    \ Walker White \n\n\
    \ Jansen's dining room \n\n\
    \ Candlestick \n\n\
    \ Law Library \n\n\
    \ Ezra Cornell\n";
  (match read_line () with
  | _ -> print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "    \
     |-----------------------------------------------------------------|\n\
    \    |                |            |          |         \
     |              |\n\
    \    |    COCKTAIL    |            |   HALL   |         |  \
     DUFFIELD    |\n\
    \    |     LOUNGE     |            |     ۩    |         \
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
     |-----------------------------------------------------------------|\n\
    \ ۩ = You are here!\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\n\
    \ Player 3's cards: \n\n\
    \ Helen Newman \n\n\
    \ cocktail lounge \n\n\
    \ Knife \n\n\
    \ Libe Slope\n\n\
    \ Wrench \n\n\
    \ David Gries\n";
  match read_line () with
  | _ -> print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

let rec choose_char n =
  if n > 1 then choose_char (n - 1)
  else
    print_endline
      "\n\
      \  It's time to choose a character! \n\
      \  Who do you want to be? Have each player press a different  \
       number from\n\
      \   1-6\n\
      \  then write down the names of your characters";
  let p = read_int () in
  if p = 1 then print_endline "You are Martha Pollack"
  else if p = 2 then print_endline "You are Ezra Cornell"
  else if p = 3 then print_endline "You are David Gries"
  else if p = 4 then print_endline "You are Michael Clarkson"
  else if p = 5 then print_endline "You are Walker White"
  else if p = 6 then print_endline "You are Claire Cardie"

let user_info id =
  let x = string_of_int id in
  print_endline
    ("User" ^ x ^ ": Are you Ready? \n You are Michael Clarkson")

let rec get_num_players () =
  print_endline "How many players are there?";
  let n = read_line () |> int_of_string in
  if n > 6 then (
    print_endline "That's too many players! Try again.";
    get_num_players ())
  else if n < 3 then (
    print_endline "Not enough players! Try again.";
    get_num_players ())
  else choose_char n

(* num_players := n; user_info 1; hands_for_players () *)

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
    "Sorry, you lost. The solution was [display solution]. Try again \
     next time"

let rec user_turn () =
  (* print user's cards * print board and location on it * roll die move
     to a room * end turn by guessing or accusing *)
  match Command.parse (read_line ()) with
  | Giveup ->
      (* failwith "TODO: display solution"; *)
      end_game ()
  | Guess s -> failwith "TODO: let user accuse someone"
  | Accuse s -> end_game ()
  | Help ->
      show_help ();
      user_turn ()
  | Go s ->
      (* failwith "TODO: let the user go to the room"; *)
      user_turn ()

(* | _ -> failwith "TODO: call another function that lets the user go
   on\n\ \ with the game" *)

let solution = Clue.random_solution

(* let user_hands () = user_hands_var := draw_hands []; user_turn () *)
