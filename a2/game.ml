open Yojson.Basic.Util ;;
exception Rooms_parse_error;;
exception Items_parse_error;;


type room = {
  id : string;
  description : string;
  items : string list;
  points : int;
  treasure : string list;
  exits : (string * room) list;
  }

type item = {
  id : string;
  description : string;
  points : int;
}

type state = {
  room : string;
  items : string;
  points : int;
}

let parse_commands commands =
let second_input =
  match commands with
  |[] -> ""
  |h::m::t -> m
  |h::[] -> ""
  (*WHAT ABOUT MATCHING WRONG ARGS AND ERRORS*)
in Yojson.Basic.from_file(second_input)

let extract_rooms (t : Yojson.Basic.json) : room list =
  try
    let raw = [t]
      |> filter_member "rooms"
      |> flatten
    in List.map (fun ( r : Yojson.Basic.json) -> {
      id = r |> member "id" |> to_string;
      description = r |> member "description" |> to_string;
      items = [r] |> filter_member "items" |> flatten |> filter_string;
      points = r |> member "points" |> to_int;
      treasure = [r] |> filter_member "treasure" |> flatten |> filter_string;
      exits = []
    }) raw
  with _ -> raise Rooms_parse_error

let update_rooms (t : Yojson.Basic.json) (r : room list) : room list=
  let raw = [t]
    |> filter_member "rooms"
    |> flatten
  in let direction_room = List.map
        (fun ( s : Yojson.Basic.json) ->
        (s |> member "direction" |> to_string, s |> member "room" |> to_string))
        (raw |> filter_member "exits" |> flatten)
  in let map_room (direction, room_id) =
        let room = List.find
          (fun (r: room) -> r.id = room_id) r in
          (direction, room) in
  let new_exits: (string* room) list = List.map map_room direction_room in
   List.map (fun room -> {room with exits = new_exits}) r

let extract_items (t : Yojson.Basic.json) : item list =
  try
    let raw = [t]
      |> filter_member "items"
      |> flatten
    in List.map (fun ( r : Yojson.Basic.json) -> {
      id = r |> member "id" |> to_string;
      description = r |> member "description" |> to_string;
      points = r |> member "points" |> to_int;
    }) raw
  with _ -> raise Items_parse_error

  let update_items (t : Yojson.Basic.json) (r : item list) : room list=
    let raw = [t]
      |> filter_member "items"
      |> flatten
    in let item_list = List.map
        (fun ( s : Yojson.Basic.json) ->
        (s |> member "item" |> to_string, s |> member "room" |> to_string))
        (raw |> filter_member "exits" |> flatten)
    in let map_room (direction, room_id) =
        let room = List.find
          (fun (r: room) -> r.id = room_id) r in
          (direction, room) in
    let new_exits: (string* room) list = List.map map_room direction_room in
    List.map (fun room -> {room with exits = new_exits}) r

(*let inputs = parse_commands (Array.to_list Sys.argv) in
  let raw_rooms = extract_rooms inputs in
    update_rooms inputs raw_rooms*)

let test = parse_commands (Array.to_list Sys.argv) in
  raw_items = extract_items test

