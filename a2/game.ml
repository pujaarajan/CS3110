open Yojson.Basic.Util ;;
exception Rooms_parse_error;;
exception Items_parse_error;;
exception Start_room_parse_error

type item = {
  id : string;
  description : string;
  points : int;
}

type room = {
  id : string;
  description : string;
  items : item list;
  points : int;
  treasure : item list;
  exits : (string * room) list;
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
      items =
        List.map (fun ( s : Yojson.Basic.json) -> {
            id = s |> member "id" |> to_string;
            description = s |> member "description" |> to_string;
            points = s |> member "points" |> to_int;
            }) (r |> member "items" |> to_list);
      points = r |> member "points" |> to_int;
      treasure =
          List.map (fun ( s : Yojson.Basic.json) -> {
            id = s |> member "id" |> to_string;
            description = s |> member "description" |> to_string;
            points = s |> member "points" |> to_int;
            }) (r |> member "treasure" |> to_list);
      exits = [];
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
  let new_exits: (string * room) list = List.map map_room direction_room in
   List.map (fun room -> {room with exits = new_exits}) r

let inputs = parse_commands (Array.to_list Sys.argv) in
  let raw_rooms = extract_rooms inputs in
    update_rooms inputs raw_rooms

let start_room (t : Yojson.Basic.json) (r : room list) : room =
  let start_room_id =
    t |> member "start_room" |> to_string
  in List.find
  (fun (r: room) -> r.id = start_room_id) r

(*create current game state*)
let game_state = {
  room : string;
  items : string;
  points : int;
}
