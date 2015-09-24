open Yojson.Basic.Util
open Str
exception Extract_rooms_error
exception Extract_items_error

(*OTHER*)

(*Real World Ocaml*)
let rec sum l =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl

let rec add_s_to_l (l: string list) (s: string) =
  match l with
  | [] -> [s]
  | h::t -> h::(add_s_to_l t s)

(*ROOM*)

type room = {
  room_id : string;
  room_description : string;
  room_items : string list;
  room_points : int;
  room_treasure : string list ;
  room_exits : (string * string) list;
  }

let extract_exits (t : Yojson.Basic.json) : (string * string) list =
   List.map (fun a -> member "direction" a |> to_string, member "room" a |> to_string) (t |> member "exits" |> to_list)

let extract_rooms (t : Yojson.Basic.json) : room list =
  try
    let raw = [t]
      |> filter_member "rooms"
      |> flatten
    in List.map (fun ( r : Yojson.Basic.json) -> {
      room_id = r |> member "id" |> to_string;
      room_description = r |> member "description" |> to_string;
      room_items = [r] |> filter_member "items" |> flatten |> filter_string;
      room_points = r |> member "points" |> to_int;
      room_treasure = [r] |> filter_member "treasure" |> flatten |> filter_string;
      room_exits = extract_exits r
    }) raw
  with _ -> raise Extract_rooms_error

let string_to_room (room_id : bytes) (r : room list) : room =
  List.find(fun (r: room) -> r.room_id = room_id) r

(*ITEM*)

type item = {
  item_id : string;
  item_description : string;
  item_points : int;
}

let extract_items (t : Yojson.Basic.json) : (string * item) list =
  try
    let raw = [t]
      |> filter_member "items"
      |> flatten
    in List.map (fun ( r : Yojson.Basic.json) -> (r |> member "id" |> to_string, {
      item_id = r |> member "id" |> to_string;
      item_description = r |> member "description" |> to_string;
      item_points = r |> member "points" |> to_int;
    })) raw
  with _ -> raise Extract_items_error

let string_to_item (item_id : bytes) (i : item list) : item =
  List.find(fun (i : item) -> i.item_id = item_id) i


(*TAKE*)

(*HELP - WHY DO I NEED AN IN IN ALL 3 OF THESE*)
let takeitem_updateplayer (player : player) (item_id : string) : player =
  if List.exists (fun (s: string) -> s = item_id) player.player_items then
    let () = print_string("You already have that item.\n") in player
    else
    let () = { player with player_items = (add_s_to_l player.player_items item_id); } in player

let takeitem_updaterooms (rooms : room) (item_id : string) : room =
  if List.exists (fun (s: string) -> s = item_id) rooms.room_items then
    let () = print_string("You already have that item.\n") in rooms
    else
    let () = { room with room_items = List.filter (fun (s: string) -> s <> item_id) rooms.room_items; }

let takeitem_updategame (game_now : game_now) (item_id : string) : game_now =
  if List.exists (fun (s: string) -> s = item_id) game_now.your_items then
    let () = print_string("You already have that item.\n") in game_now
    else
    let item_point = (string_to_item item_id).item_points in
    { game_now with your_items = (game_now.your_items@[item_id]); room_points = sum(game_now.your_points@[item_point]) }

(*DROP*)

let dropitem_updateplayer (player : player) (item_id : string) : player =
  if List.exists (fun (s: string) -> s = item_id) player.player_items then
    let () = { player with player_items = List.filter (fun (s: string) -> s <> item_id) player.player_items; }
    in player
    else
    let () = print_string("You don't have that item to drop.\n") in player

let dropitem_updaterooms (rooms : room) (item_id : string) : room =
  if List.exists (fun (s: string) -> s = item_id) rooms.room_items then
    let () = { room with room_items = List.filter (fun (s: string) -> s <> item_id) rooms.room_items; }
    else
    let () = print_string("You don't have that item to drop.\n") in rooms


(*PLAYER*)

type player = {
  player_room : string;
  player_items : string list;
  player_win : bool;
}

let player (t : Yojson.Basic.json) : player = {
  player_room = t |> member "start_room" |> to_string;
  player_items = [t] |> filter_member "start_items" |> flatten |> filter_string;
  player_win = false;
  }

(*GAME NOW*)

type game_now = {
  your_points : int;
  your_items : string list;
  your_rooms : string list;
  your_turns : int;
}

let game_now : game_now = {
  your_rooms = [];
  your_items = [];
  your_points = 0;
  your_turns = 0;
}

(*GAME ALL*)

type game_all = {
  all_points : int;
  all_items : string list;
  all_rooms : string list;
}

let game_all (t : Yojson.Basic.json) : game_all = {
  all_rooms = [t] |> filter_member "rooms" |> flatten |> filter_member "id" |> filter_string;
  all_items = [t] |> filter_member "items" |> flatten |> filter_member "id" |> filter_string;
  all_points =
  let room_points = [t] |> filter_member "rooms"|> flatten
    |> filter_member "points" |> filter_int in
    let item_points = [t] |> filter_member "items"|> flatten
    |> filter_member "points" |> filter_int in
    sum(room_points@item_points);
    }

(*Test functions*)

(*let file_input = String.lowercase(String.trim(Sys.argv.(1))) in
  Yojson.Basic.from_file file_input*)
let parse_commands commands =
  let second_input =
  match commands with
  |[] -> ""
  |h::m::t -> m
  |h::[] -> ""
  (*WHAT ABOUT MATCHING WRONG ARGS AND ERRORS*)
in Yojson.Basic.from_file(second_input)

let inputs = parse_commands (Array.to_list Sys.argv)
let repl_rooms = extract_rooms inputs
let items = extract_items inputs
let repl_player = player(inputs)
let repl_game_all = game_all(inputs)
let repl_game_now = game_now


(*REPL LOOP*)

let rec repl (player : player) (rooms : room list) (game_now : game_now) (game_all : game_all) : unit =
  if (game_now.your_points = game_all.all_points) then
    print_string("You win!")
  else
    let () = print_string("What do you want to do now?: \n") in
    let input = String.lowercase(String.trim(read_line())) in
    let input_words = Str.split (Str.regexp "[ \t]+") input in
    match input_words with
    | ["inventory";] | ["inv";] ->
      let () = List.iter (Printf.printf "%s \n") (repl_player.player_items) in
      repl player rooms game_now game_all
    | ["score";] ->
      let () = print_int game_now.your_points in
      repl player rooms game_now game_all
    | ["turns";] ->
      let () = print_int game_now.your_turns in
      repl player rooms game_now game_all
    | ["quit";] ->
      let () = print_string("Quitting the game...\n") in
      exit 0
    | ["look";] ->
      let () = print_endline (string_to_room repl_player.player_room (repl_rooms)).room_description in
      repl player rooms game_now game_all
    | _ ->
    let () = print_string("Not a real function\n") in
    repl player rooms game_now game_all


let () = repl repl_player repl_rooms repl_game_now repl_game_all

