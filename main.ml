open Command
open Clue
open ANSITerminal
open Game_state2

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
    \ Welcome to our game of Clue! \n\
    \  \n\
    \ Someone has been murdered on this campus... \n\
    \  \n\
    \ it's your job to find out the murderer, place of murder, and \
     weapon of choice.";
  print_endline "\n Press enter to continue.";
  match read_line () with
  | _ ->
      let num_players = Game_state2.start_rules () in
      Game_state2.user_turn num_players Player_state.initial_player
        Player_state.player_options

(* Start the game engine *)
let () = main ()
