open Yojson.Basic.Util
open Str
exception Extract_rooms_error
exception Extract_items_error

(*RANDOM USEFUL FUNCTIONS *)

(* Sums a list of integers. From Real World Ocaml. *)
let rec sum l =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl

(* Adds a string to a string list. *)
let rec add_s_to_l (l: string list) (s: string) =
  match l with
  | [] -> [s]
  | h::t -> h::(add_s_to_l t s)

(* TYPE ROOM AND RELATED ROOM FUNCTIONS *)

(* Room type includes a room id, description about itself, a list of the items
 * in it, the number of points it is worth, and all of the possible exits *)
type room = {
  room_id : string;
  room_description : string;
  room_items : string list;
  room_points : int;
  room_treasure : string list ;
  room_exits : (string * string) list;
  }

(* Extracts exits from Json file and creates a list with elements in
 * (direction, room) format *)
let extract_exits (t : Yojson.Basic.json) : (string * string) list =
  List.map (fun a -> member "direction" a
  |> to_string, member "room" a |> to_string)
  (t |> member "exits" |> to_list)

(* Extracts rooms from Json file and creates list with elements of type room *)
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

(*Finds room record in a list of rooms using the string of a room id*)
let string_to_room (room_id : bytes) (r : room list) : room =
  List.find(fun (r: room) -> r.room_id = room_id) r

(* TYPE ITEM AND RELATED ITEM FUNCTIONS *)

(* Item type includes an item id, description about itself, and
 * the number of points it is worth*)
type item = {
  item_id : string;
  item_description : string;
  item_points : int;
}

(* Extracts items from Json file and creates list with elements of type item *)
let extract_items (t : Yojson.Basic.json) : item list =
  try
    let raw = [t]
      |> filter_member "items"
      |> flatten
    in List.map (fun ( r : Yojson.Basic.json) -> {
      item_id = r |> member "id" |> to_string;
      item_description = r |> member "description" |> to_string;
      item_points = r |> member "points" |> to_int;
    }) raw
  with _ -> raise Extract_items_error


(*Finds item record in a list of items using the string of a item id*)
let string_to_item (item_id : bytes) (i : item list) : item =
  List.find(fun (i : item) -> i.item_id = item_id) i

(*PLAYER TYPE AND RELATED FUNCTIONS*)

(*Player type includes the room the player is in now, and the items in the
 * players inventory now. *)
type player = {
  player_room : string;
  player_items : string list;
}

(*Creates the player that starts with information about the player*)
let player (t : Yojson.Basic.json) : player = {
  player_room = t |> member "start_room" |> to_string;
  player_items = [t] |> filter_member "start_items" |> flatten |> filter_string;
  }

(*GAME NOW AND RELATED FUNCTIONS*)

(*Game now type is current status of the game including the number of points
 * you got, items you have, rooms you you visited, and the number of turns you
 * have used*)
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

(* Increments the player's number of turns by one *)
let increase_turns (game_now : game_now) : game_now =
  { game_now with your_turns = (game_now.your_turns + 1); }

(*GAME ALL AND RELATED FUNCTIONS*)

(*Game all type saves the information about the game as a whole, including
 * total points available, all the items to get, and all the rooms to go to *)
type game_all = {
  all_points : int;
  all_items : string list;
  all_rooms : string list;
}

(*Creates the game by saving all of the information about the whole game from
 * the Json file *)
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

(*MORE GAME FUNCTIONS*)

(* Updates the player by adding the new item taken to the inventory *)
let takeitem_updateplayer (player : player) (item_id : string) : player =
  if List.exists (fun (s: string) -> s = item_id) player.player_items then
    let () = print_string("You already have that item.\n") in player
  else
    { player with player_items = (add_s_to_l player.player_items item_id); }

(*Updates the room by removing the item from the room items after taken*)
let takeitem_updaterooms (room_id : string) (item_id : string)
(room_list : room list) : room list =
  let room_record = string_to_room room_id room_list in
    if List.exists (fun (s: string) -> s = item_id) room_record.room_items then
      let () = print_string("You already have that item.\n") in room_list
    else
      let new_room = { room_record with room_items =
        List.filter (fun (s: string) -> s <> item_id) room_record.room_items; } in
          List.map (fun x -> if x.room_id = room_id then new_room else x ) room_list

(*Updates the game by adding the item to the all the items you took after taken*)
let takeitem_updategame (game_now : game_now) (item_id : string)
(item_list : item list) : game_now =
  if List.exists (fun (s: string) -> s = item_id) game_now.your_items then
    let () = print_string("You already have that item.\n") in game_now
  else
    { game_now with your_items = (game_now.your_items@[item_id]); }

(* Updates the player by removing the old item dropped from the inventory *)
let dropitem_updateplayer (player : player) (item_id : string) : player =
  if List.exists (fun (s: string) -> s = item_id) player.player_items then
    { player with player_items =
    List.filter (fun (s: string) -> s <> item_id) player.player_items; }
  else
    let () = print_string("You don't have that item to drop.\n") in player

(*Updates the room by adding the item to the room items after dropped*)
let dropitem_updaterooms (room_id : string) (item_id : string)
(room_list : room list) : room list=
  let room_record = string_to_room room_id room_list in
    if List.exists (fun (s: string) -> s <> item_id) room_record.room_items then
      let new_room = { room_record with room_items =
      add_s_to_l room_record.room_items item_id} in
        List.map (fun x -> if x.room_id = room_id then new_room else x )
        room_list
    else
      let () = print_string("That item is already in the room.\n") in room_list

(* Move player to new room using the direction input *)
let go_updateplayer (direction: string) (player: player)
(room_list : room list) : player =
  let room_now = string_to_room player.player_room room_list in
  if List.exists (fun (s: string*string) -> fst s = direction)
  room_now.room_exits then
    let direction_room = List.find (fun (s: string*string) -> fst s = direction)
    room_now.room_exits in
      { player with player_room = (snd direction_room) ; }
  else
    let () = print_string ("Not a valid function\n") in player

(* Update the game after player moves to the new room using the
 * direction input *)
let go_updategame (direction:string) (player:player) (game_now:game_now)
(room_list : room list) : game_now =
  let room_now = string_to_room player.player_room room_list in
    if List.exists (fun (s: string*string) -> fst s = direction)
    room_now.room_exits then
      let direction_room =
      List.find (fun (s: string*string) -> fst s = direction)
      room_now.room_exits in
        if List.exists (fun (s: string) -> s <> (snd direction_room))
        game_now.your_rooms then
          { game_now with your_rooms = (game_now.your_rooms@[(snd direction_room)]); }
        else game_now
    else game_now

(* OTHER TESTING FUNCTIONS *)

(* Parses the input commands *)
let parse_commands commands =
  let second_input =
  match commands with
  |[] -> ""
  |h::m::t -> m
  |h::[] -> ""
  (*WHAT ABOUT MATCHING WRONG ARGS AND ERRORS*)
in Yojson.Basic.from_file(second_input)

(* Gets inputs from the command line *)
let inputs = parse_commands (Array.to_list Sys.argv)

(* Gets rooms input for the repl *)
let repl_rooms = extract_rooms inputs

(* Gets items input for the repl *)
let repl_items = extract_items inputs

(* Gets player input for the repl *)
let repl_player = player(inputs)

(* Gets all of the game's input for the repl *)
let repl_game_all = game_all(inputs)

(* Gets the start game's input for the repl *)
let repl_game_now = game_now

(* REPL *)

(*A recursive repl which runs the game until you quit or win*)
let rec repl (player : player) (rooms : room list) (items : item list) (game_now : game_now) (game_all : game_all) : unit =
  if (game_now.your_points = game_all.all_points) then
    print_string("You win!")
  else
    let () = print_string("What do you want to do now?: \n") in
    let input = String.lowercase(String.trim(read_line())) in
    let input_words = Str.bounded_split (Str.regexp "[ \t]+") input 2 in
    match input_words with
    | [h] -> begin
      match h with
        |"inv" ->
        let () = List.iter (Printf.printf "%s \n") (repl_player.player_items) in
        repl player rooms items (increase_turns(game_now)) game_all
        |"inventory" -> let () = List.iter (Printf.printf "%s \n") (player.player_items) in
        repl player rooms items (increase_turns(game_now)) game_all
        | "score" ->
        let () = print_string ((string_of_int game_now.your_points) ^ "\n") in
        repl player rooms items (increase_turns(game_now)) game_all
        | "turns" ->
        let () = print_string ((string_of_int game_now.your_turns) ^ "\n") in
        repl player rooms items (increase_turns(game_now)) game_all
        | "quit" ->
        let () = print_string("Quitting the game...\n") in
        exit 0
        | "look" ->
        let () = print_endline (string_to_room player.player_room (rooms)).room_description in
        repl player rooms items (increase_turns(game_now)) game_all
        | _ ->
        let player = go_updateplayer h player rooms in
            let game_now = go_updategame h player game_now rooms in
            repl player rooms items (increase_turns(game_now)) game_all
      end
    | [h;t] -> begin
        (*If t in list then do this*)
        match h with
          | "take" ->
              let player = takeitem_updateplayer player t in
              let rooms = takeitem_updaterooms player.player_room t rooms in
              let game_now = takeitem_updategame game_now t items in
              repl player rooms items (increase_turns(game_now)) game_all
          | "drop" ->
              let player = dropitem_updateplayer player t in
              let rooms = dropitem_updaterooms player.player_room t rooms in
              repl player rooms items (increase_turns(game_now)) game_all
          | "move" ->
            let player = go_updateplayer t player rooms in
            let game_now = go_updategame t player game_now rooms in
            repl player rooms items (increase_turns(game_now)) game_all
          | _ ->
          let () = print_string("Not a real function\n") in
          repl player rooms items (increase_turns(game_now)) game_all
        end
    | _ -> let () = print_string("Not a real function\n") in
    repl player rooms items (increase_turns(game_now)) game_all

(* Runs the repl with inputs*)
let () = repl repl_player repl_rooms repl_items repl_game_now repl_game_all

