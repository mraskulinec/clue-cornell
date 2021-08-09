open Clue2

type p = {
  name : Clue2.player_name;
  visited : Clue2.room_id list;
}

let make_p n cr = { name = n; visited = [] }

let initial_player =
  { name = pn_of_s "Martin Y. Tang"; visited = [ id_of_i 6 ] }

let player_options =
  [
    { name = pn_of_s "Martin Y. Tang"; visited = [ id_of_i 6 ] };
    { name = pn_of_s "Goldwin Smith"; visited = [ id_of_i 6 ] };
    { name = pn_of_s "Mann"; visited = [ id_of_i 6 ] };
    { name = pn_of_s "Happy Dave"; visited = [ id_of_i 6 ] };
    { name = pn_of_s "G.K. Chesterton"; visited = [ id_of_i 6 ] };
    { name = pn_of_s "Charles Dickens"; visited = [ id_of_i 6 ] };
  ]

(* [change_options opt nv p rf i acc name] is the player_options list
   that accounts for the most recently visited room *)
let rec change_options options new_visit p rf i acc name =
  match options with
  | [] -> (acc : p list)
  | hd :: tl ->
      if hd.name = name then
        let new_acc =
          acc @ [ { name = hd.name; visited = new_visit } ]
        in
        change_options tl new_visit p rf i new_acc name
      else
        let new_acc =
          acc @ [ { name = hd.name; visited = hd.visited } ]
        in
        change_options tl new_visit p rf i new_acc name

let change_room (p : p) (rf : room_id) i name =
  let new_visit = rf :: (List.nth player_options i).visited in
  change_options player_options new_visit p rf i [] name

let rec get_player_name i p = (List.nth p (i - 1)).name

let name_of_p p = p.name

let room_of_p p = List.hd p.visited
