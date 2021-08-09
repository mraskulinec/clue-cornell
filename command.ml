type room_phrase = string list

type guess_phrase = string list

type command =
  | Go of room_phrase
  | Accuse of guess_phrase
  | Guess of guess_phrase
  | Next
  | Giveup
  | Help

exception Empty

exception Malformed

(** [split_phrase s] creates a list of strings from the user's input
    string s *)
let split_phrase s =
  String.split_on_char ' ' s |> List.filter (fun x -> x <> "")

let parse str =
  match split_phrase str with
  | [ "help" ] -> Help
  | [ "give"; "up" ] -> Giveup
  | [ "next" ] -> Next
  | "accuse" :: t -> if t <> [] then Accuse t else raise Malformed
  | "guess" :: t -> if t <> [] then Guess t else raise Malformed
  | "go" :: t -> if t <> [] then Go t else raise Malformed
  | [] -> raise Empty
  | _ -> raise Malformed
