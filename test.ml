open OUnit2
open Command
open Clue2
open Rooms
open Game_state2
open Player_state

(* APPROACH TO TESTING: We decided to test our Command, Game State 2,
   and Rooms compilation units. We did a combination of test-driven
   development, glass-box testing, testing by playing on the user
   interface, and testing on the utop command line.

   For Command, we tested using TDD because there was a point in which
   we didn't yet have our command functions linked to the interface. The
   tests that involve parsing commands are meant to make sure that the
   user is typing a valid input into the system. We used glass-box
   testing for our Command compilation unit.

   We omitted some test cases from Rooms because they were duplicated in
   Game State 2. Since some of the functions could not be tested
   directly, we wrote test functions that would test the end result,
   which shows our program correctness.

   For Clue 2, we mostly tested our randomization functions through the
   utop command line because it was impossible to predict, for example,
   which cards would be randomly generated for a solution.

   For Game State 2, we tested the functions that would not be shown in
   the user interface. This means we omitted functions including
   [show_board ()], [show_help()], [user_turn()] because we could
   identify whether or not they were showing up in the terminal. We also
   omitted *)

(** [parse_test name str expected] is an OUnit test case named [name]
    for [Command.parse str] asserting that the output is [expected]. *)
let parse_test (name : string) (str : string) (expected : command) :
    test =
  name >:: fun _ -> assert_equal expected (parse str)

let command_tests =
  [
    parse_test "help command" "help" Help;
    parse_test "next command" "next" Next;
    parse_test "give up command" "give up" Giveup;
    parse_test "the hall" " go the hall " (Go [ "the"; "hall" ]);
    parse_test "libe slope" "    go    libe slope "
      (Go [ "libe"; "slope" ]);
    parse_test "basic guess" "guess Michael Clarkson revolver the hall"
      (Guess [ "Michael"; "Clarkson"; "revolver"; "the"; "hall" ]);
    parse_test "another guess"
      "guess Martha Pollack revolver libe slope"
      (Guess [ "Martha"; "Pollack"; "revolver"; "libe"; "slope" ]);
    parse_test "basic accusation"
      "accuse David Gries cocktail lounge sword "
      (Accuse [ "David"; "Gries"; "cocktail"; "lounge"; "sword" ]);
    parse_test "second accusation"
      "accuse Walker White bailey hall knife "
      (Accuse [ "Walker"; "White"; "bailey"; "hall"; "knife" ]);
    ("empty str" >:: fun _ -> assert_raises Empty (fun () -> parse " "));
    ( "empty spaces" >:: fun _ ->
      assert_raises Empty (fun () -> parse "   ") );
    ( "just go" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "go ") );
    ( "double help" >:: fun _ ->
      assert_raises Malformed (fun () -> parse " help help") );
    ( "malformed str" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "Martha Pollack") );
    ( "malformed room/person" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "Michael Clarkson knife")
    );
    ( "just guess" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "guess ") );
    ( "just revolver" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "revolver ") );
    ( "just a room" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "jansen's dining ") );
    ( "malformed accuse" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "accuse") );
    ( "malformed next" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "next help") );
    ( "give up malformed" >:: fun _ ->
      assert_raises Malformed (fun () -> parse " give up today") );
  ]

(** [start_room_test name input expected] is an OUnit test case named
    [name] for [Room.start_room input] asserting that the output is
    [expected]. *)
let start_room_test
    (name : string)
    (input : Rooms.r)
    (expected : string) : test =
  name >:: fun _ -> assert_equal expected (start_room input)

(** [room_names_test name input expected] is an OUnit test case named
    [name] for [Room.room_names input] asserting that the output is
    [expected]. *)
let room_names_test
    (name : string)
    (input : Rooms.r)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (room_names input)

(** [exits_test name j room expected] is an OUnit test case named [name]
    for [Room.exits input] asserting that the output is [expected]. *)

(* let exits_test (name : string) (j : Rooms.r) (room : string)
   (expected : string list) : test = name >:: fun _ -> assert_equal
   expected (exits j room) *)

let file = Yojson.Basic.from_file "rooms.json" |> from_json

let room_tests =
  [
    start_room_test "basic start room (hall)" file "the hall";
    room_names_test "all room ids" file
      [
        "the hall";
        "cocktail lounge";
        "jansen's dining";
        "terrace restaurant";
        "bailey hall";
        "libe slope";
        "helen newman";
        "law library";
        "duffield hall";
      ];
  ]

(** [get_exits_test name rm rms expected] is an OUnit test case named
    [name] for [Rooms.get_exits_of_r r rm rms] asserting that the output
    is [expected]. *)
let get_exits_test
    (name : string)
    (rm : string)
    (rms : Rooms.room list)
    (expected : Rooms.exit list) : test =
  name >:: fun _ -> assert_equal expected (get_exits_of_r rm rms)

(** [get_rid_tests name rm rms expected] is an OUnit test case named
    [name] for [Rooms.get_room_id_of_r r rm rms] asserting that the
    output is [expected]. *)
let get_rid_tests
    (name : string)
    (rm : string)
    (rooms : Rooms.room list)
    (expected : int) : test =
  name >:: fun _ -> assert_equal expected (get_room_id_of_r rm rooms)

(** [get_rid_tests name rf exits expected] is an OUnit test case named
    [name] for [Game_state2.check_exit rf exits] asserting that the
    output is [expected]. *)
let check_exit_tests
    (name : string)
    (rf : string)
    (exits : Rooms.exit list)
    (expected : bool) : test =
  name >:: fun _ -> assert_equal expected (check_exit rf exits)

(** [user_index_test name n p expected] is an OUnit test case named
    [name] for [Game_state2.get_user_index p] asserting that the output
    is [expected]. *)
let user_index_test
    (name : string)
    (player_name : string)
    (expected : int) : test =
  name >:: fun _ -> assert_equal expected (get_user_index player_name)

let game_state_tests =
  [
    (* (let f () = get_exits_of_r "the hall" (get_rooms file) in
       OUnit2.assert_raises (Failure "not a valid exit of current room")
       f; *)
    get_exits_test "hall exits" "the hall" (get_rooms file)
      [
        make_exit "cocktail lounge" "regular";
        make_exit "duffield hall" "regular";
      ];
    get_exits_test "duffield exits" "duffield hall" (get_rooms file)
      [
        make_exit "the hall" "regular";
        make_exit "law library" "regular";
      ];
    get_exits_test "terrace exits" "terrace restaurant" (get_rooms file)
      [
        make_exit "bailey hall" "regular";
        make_exit "jansen's dining" "regular";
      ];
    get_exits_test "libe exits" "libe slope" (get_rooms file)
      [
        make_exit "helen newman" "regular";
        make_exit "bailey hall" "regular";
      ];
    get_exits_test "law tests" "law library" (get_rooms file)
      [
        make_exit "duffield hall" "regular";
        make_exit "helen newman" "regular";
      ];
    get_rid_tests "hall is 0" "the hall"
      [
        make_room "the hall" 0
          [
            make_exit "cocktail lounge" "regular";
            make_exit "duffield hall" "regular";
          ];
        make_room "cocktail lounge" 1
          [
            make_exit "jansen's dining" "regular";
            make_exit "the hall" "regular";
          ];
        make_room "jansen's dining" 2
          [
            make_exit "terrace restaurant" "regular";
            make_exit "cocktail lounge" "regular";
          ];
      ]
      0;
    get_rid_tests "jansen's is 2" "jansen's dining"
      [
        make_room "the hall" 0
          [
            make_exit "cocktail lounge" "regular";
            make_exit "duffield hall" "regular";
          ];
        make_room "cocktail lounge" 1
          [
            make_exit "jansen's dining" "regular";
            make_exit "the hall" "regular";
          ];
        make_room "jansen's dining" 2
          [
            make_exit "terrace restaurant" "regular";
            make_exit "cocktail lounge" "regular";
          ];
      ]
      2;
    check_exit_tests "first valid exit" "terrace restaurant"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
      ]
      true;
    check_exit_tests "third invalid exit" "the cool place"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
        make_exit "cool place" "regular";
      ]
      false;
    check_exit_tests "third invalid exit" "new jersey"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
        make_exit "cool place" "regular";
      ]
      false;
    check_exit_tests "another valid exit" "terrace restaurant"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
      ]
      true;
    check_exit_tests "no exits" "terrace restaurant" [] false;
    check_exit_tests "invalid exit in general" "martha pollack"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
      ]
      false;
    check_exit_tests "invalid exit from this room" "jansen's dining"
      [
        make_exit "terrace restaurant" "regular";
        make_exit "cocktail lounge" "regular";
      ]
      false;
    user_index_test "Happy Dave at index 3" "Happy Dave" 3;
    user_index_test "G.K. at index 4" "G.K. Chesterton" 4;
    user_index_test "Tang at index 0" "Martin Y. Tang" 0;
    user_index_test "GSH at index 1" "Goldwin Smith" 1;
    user_index_test "Mann at index 2" "Mann" 2;
    user_index_test "Random person" "Hello" 5;
  ]

(** [card_name_test name n c expected] is an OUnit test case named
    [name] for [Clue2.card_name c] asserting that the output is
    [expected]. *)
let card_name_test (name : string) (c : Clue2.card') (expected : string)
    : test =
  name >:: fun _ -> assert_equal expected (card_name c)

(** [card_id_test name n c expected] is an OUnit test case named [name]
    for [Clue2.card_name c] asserting that the output is [expected]. *)
let card_id_test (name : string) (c : Clue2.card') (expected : int) :
    test =
  name >:: fun _ -> assert_equal expected (card_id c)

(** [randomize_game_test name p lst acc expected] is an OUnit test case
    named [name] for [Clue2.randomize_game n p lst acc] asserting that
    the output is [expected]. *)
let randomize_game_test
    (name : string)
    (np : int)
    (pleft : int)
    (lst : card' list)
    (lstsacc : card' list list)
    (expected : card' list list) : test =
  name >:: fun _ ->
  assert_equal expected (randomize_game np pleft lst lstsacc)

let clue2_tests =
  [
    card_name_test "example room name"
      (Clue2.make_card "fake room" 15 "room")
      "fake room";
    card_name_test "example weapon card"
      (Clue2.make_card "examplew" 20 "weapon")
      "examplew";
    card_name_test "example character card"
      (Clue2.make_card "martha p2" 20 "character")
      "martha p2";
    card_id_test "example room id"
      (Clue2.make_card "room ex" 0 "room")
      0;
    card_id_test "example weapon id"
      (Clue2.make_card "weaponex" 5 "weapon")
      5;
    card_id_test "example character id"
      (Clue2.make_card "martha p2" 10 "character")
      10;
    randomize_game_test "1 left" 3 1
      [ Clue2.make_card "weaponex" 5 "weapon" ]
      [] [ [] ];
  ]

let player_state_tests =
  [
    ( "name a rand player" >:: fun _ ->
      assert_equal
        (name_of_p
           (Player_state.make_p (pn_of_s "Martin Y. Tang") (id_of_i 0)))
        (pn_of_s "Martin Y. Tang") );
  ]

let suite =
  "tests for final project"
  >::: List.flatten
         [
           command_tests;
           room_tests;
           clue2_tests;
           game_state_tests;
           player_state_tests;
         ]

let _ = run_test_tt_main suite
