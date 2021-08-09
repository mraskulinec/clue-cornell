open Yojson.Basic.Util
open Map
open List

type room_id = string

type exit_name = string

type exit = {
  name : exit_name;
  typ : string;
}

type room = {
  name : string;
  id : int;
  exits : exit list;
}

type r = {
  start : string;
  rooms : room list;
}

let make_exit n t = { name = n; typ = t }

let make_room r id ex = { name = r; id; exits = ex }

let start_room rms = rms.start

let exit_name e = e.name

let exit_name_of_exit (e : exit) = e.name

let room_name rm = rm.name

let get_exits rm = rm.exits

let get_rooms r = r.rooms

let get_room_id rm = rm.id

let room_names rms = List.map (fun x -> x.name) rms.rooms

(** [helper_exit j] is the exit type specified by a json file *)
let helper_exit json : exit =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    typ = to_string (List.assoc "type" (to_assoc json));
  }

(** [helper_exit_list j] is the exit list specified by a list of jsons *)
let rec helper_exit_list json : exit list =
  match json with
  | [] -> []
  | hd :: tl -> helper_exit hd :: helper_exit_list tl

(** [helper_room j] is the room type specified by json j *)
let helper_room json : room =
  {
    name = to_string (List.assoc "name" (to_assoc json));
    id = to_int (List.assoc "id" (to_assoc json));
    exits =
      json |> to_assoc |> List.assoc "exit" |> to_list
      |> helper_exit_list;
  }

(** [helper_room_list j] is the list of rooms of a json file
    representing rooms *)
let rec helper_room_list json : room list =
  match json with
  | [] -> []
  | hd :: tl -> helper_room hd :: helper_room_list tl

let from_json json : r =
  {
    start = to_string (List.assoc "start room" (to_assoc json));
    rooms =
      json |> to_assoc |> List.assoc "rooms" |> to_list
      |> helper_room_list;
  }

(** [match_room_name j room] is the room [room] in file [j] .*)
let match_room_name j room =
  try List.find (fun x -> x.name = room) j.rooms
  with Not_found -> failwith "Unimplemented"
