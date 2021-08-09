open Yojson.Basic.Util
open Map
open List

type card' = {
  name : string;
  id : int;
  typ : string;
}

let card_name c = c.name

let card_id c = c.id

type player_name = string

type room_id = int

let s_of_pn pn = pn

let pn_of_s s = s

let i_of_id id = id

let id_of_i i : room_id = i

let from_json1 json = json |> to_assoc |> List.assoc "cards" |> to_list

let from_json json_card : card' =
  {
    name = to_string (List.assoc "name" (to_assoc json_card));
    id = to_int (List.assoc "id" (to_assoc json_card));
    typ = to_string (List.assoc "type" (to_assoc json_card));
  }

let rec from_json2 json_cards : card' list =
  match json_cards with
  | [] -> []
  | h :: t -> from_json h :: from_json2 t

let from_json_final j : card' list = j |> from_json1 |> from_json2

let extract_solution lst =
  let ch = Random.int 6 in
  let ro = Random.int 10 + 6 in
  let we = Random.int 8 + 15 in
  (List.nth lst ch, List.nth lst ro, List.nth lst we)

let remove_solution (sol : card' * card' * card') lst =
  let sol_list = match sol with a, b, c -> [ a; b; c ] in
  List.filter
    (fun e ->
      e <> nth sol_list 0 && e <> nth sol_list 1 && e <> nth sol_list 2)
    lst

let rec randomize_game
    (np : int)
    (pleft : int)
    (lst : card' list)
    lstsacc =
  match pleft with
  | 0 -> lstsacc
  | i ->
      randomize_game np (pleft - 1) lst
        (List.filteri (fun i e -> i mod (np + 1) = pleft) lst :: lstsacc)

let make_card n id t = { name = n; id; typ = t }
