

(* Finding index of string in a list of string *)
let rec find_string_index_by_item (item: string) (item_set: string list): int =
    match item_set with
    | [] -> raise (Failure "Not Found")
    | h::t -> if item = h then 0 else 1 + find_string_index_by_item item t
(* Finding index of item in a list of items *)
let rec find_index_by_item (item: Item.params) (item_set: Item.params list): int =
    match item_set with
    | [] -> raise (Failure "Not Found")
    | h::t -> if item = h then 0 else 1 + find_index_by_item item t
(* Finding item given the id *)
let find_item_by_id (item_list: Item.params list) (id: string) : Item.params =
  List.find (fun (item: Item.params) -> item.id = id) item_list
(* Adding an item string to a list *)
let rec add_item_string (item_set: string list) (item: string) =
  match item_set with
  | [] -> [item]
  | h::t -> h::(add_item_string t item)
(* Removing an item string from a list *)
let rec remove_item_string (item_set: string list) (item: string)
  (index: int) : string list=
  match item_set with
  | [] -> []
  | h::t ->
    let temp_index = (find_string_index_by_item item item_set) in
    if temp_index = 0 then t else h::remove_item_string (t) (item) (temp_index-1)
(* Adding an item to a list *)
let rec add_item (item_set: Item.params list) (item: Item.params) =
  match item_set with
  | [] -> [item]
  | h::t -> h::(add_item item_set item)
(* Removing an item from a list *)
let rec remove_item (item_set: Item.params list) (item: Item.params)
  (index: int) : Item.params list =
  match item_set with
  | [] -> []
  | h::t ->
    let temp_index = (find_index_by_item item item_set) in
    if temp_index = 0 then t else h::remove_item (t) (item) (temp_index-1)



    (* Find an item given the string id *)
    let find_item_by_id (room: params) (id: string) : string =
      List.find (fun (item: string) -> item = id) room.items
    (* Player drops an item in the room *)
    let drop_item (room: params) (item:string) : params =
      { room with items = (add_item_string room.items item) }
    (* Bring score down to zero when visited*)
    let reset_score (room: params) : params =
      { room with points = 0 }
    (* Check if the id is a treasure *)
    let is_treasure (room: params) (input:string) : bool=
      List.exists (fun (item: string) -> item = input) room.treasure
    (* Check if the id is an item *)
    let has_item (room: params) (input:string) : bool=
      List.exists (fun (item: string) -> item = input) room.items
    (* Take an item from the room *)
    let take_item (room: params) (item:string) : params =
      let dropped_item = find_item_by_id room item in
      let dropped_item_index = find_string_index_by_item dropped_item room.items in
      let new_items = remove_item_string (room.items) (dropped_item) (dropped_item_index) in
      { room with items = new_items }


  }
  let find_room_by_id (game: params) (id: string) : Room.params =
    List.find (fun (room: Room.params) -> room.id = id) game.all_rooms
  let rec find_index_of_room (room_list: Room.params list) (room: Room.params) : int =
    match room_list with
    | [] -> raise (Failure "Not Found")
    | h::t -> if room = h then 0 else 1 + (find_index_of_room (t) (room))
  let find_item_by_id (game: params) (id: string) : Item.params =
    List.find (fun (item: Item.params) -> item.id = id) game.all_items
  let rec get_room_points (rooms: Room.params list) : int = match rooms with
    | [] -> 0
    | h::t -> h.points + get_room_points t
  let rec get_item_points (items: Item.params list) : int = match items with
    | [] -> 0
    | h::t -> h.points + get_item_points t
  let update_room (game: params) (room: Room.params) : params =
    let filtered_room_list = List.filter (fun (index_room: Room.params) -> not (index_room.id = room.id)) game.all_rooms in
    { game with all_rooms = room::filtered_room_list }
  let update_item (game: params) (item: Item.params) : params =
    let filtered_item_list = List.filter (fun (index_item: Item.params) -> not (index_item.id = item.id)) game.all_items in
    { game with all_items = item::filtered_item_list }
  let end_game (game:params) : params =
    { game with game_over = true}


  (* Find an item given the string id *)
  let find_item_by_id (player: params) (id: string) : Item.params =
    List.find (fun (item: Item.params) -> item.id = id) player.inventory
  (* Drop an item from the player's inventory *)
  let drop_item (player: params) (item:string) : params =
    let dropped_item = find_item_by_id player item in
    let dropped_item_index = find_index_by_item dropped_item player.inventory in
    let new_inventory = remove_item (player.inventory) (dropped_item) (dropped_item_index) in
    { player with inventory = new_inventory }
  (* Add an item into the player's inventory *)
  let add_item (game: GameState.params) (player: params) (id:string) : params =
    let item = GameState.find_item_by_id game id in
    { player with inventory = item::player.inventory }
  (* Add score to the player *)
  let add_score (player: params) (incr: int) : params =
    { player with score = player.score + incr }
  (* Add a turn to the player *)
  let add_turn (player: params) : params =
    { player with turns = player.turns + 1}
  (* Update player with his current room *)
  let update_room (player: params) (room: Room.params) : params =
    { player with room = room}
  (* Checks if player has an item *)
  let has_item (player: params) (id:string) : bool =
    List.exists (fun (item: Item.params) -> item.id = id) player.inventory



(* Print item list *)
let rec print_item_list (input_list: Item.params list) : unit =
  match input_list with
  | [] -> ()
  | h::t ->
    print_string h.id;print_string "\t";print_item_list t
(* Print string list *)
let rec print_string_list (input_list: string list) : unit =
  match input_list with
  | [] -> ()
  | h::t ->
    print_string h;print_string "\t";print_string_list t
(* Look function for a player that returns the room description where the player is *)
let look (player: Player.params) : unit =
  print_string (player.room.description);print_string("\n")
(* Move function for a player that returns the player and the game state updated*)
let move (game: GameState.params) (player: Player.params)
  (input_direction: string) : Player.params * GameState.params =
  let exits = player.room.exits in
  let bool_val = List.exists (fun (index: Exit.params) ->
    index.direction = input_direction) exits in
  if bool_val then
    let directed_room_exit =
      List.find (fun (exit: Exit.params) -> exit.direction = input_direction) exits in
    let directed_room =
      GameState.find_room_by_id game directed_room_exit.room in
    let updated_room = Room.reset_score directed_room in
    let updated_player = { player with
    room = updated_room;
    score = player.score + directed_room.points;
    } in
    (updated_player, GameState.update_room game updated_room)
    else
      let () = print_string ("You cant walk in that direction\n") in
      (Player.add_turn player, game)
(* Take function for a player to take an item from a room *)
let take (game: GameState.params) (player: Player.params) (id:string)
  : Player.params * GameState.params =
  let bool_val = Room.has_item player.room id in
  if bool_val then
    let added_item_player = Player.add_item game player id in
    let updated_room = Room.take_item player.room id in
    let score_to_subtract =
      if Room.is_treasure player.room id
      then
        let () = print_string ("You picked up a treasure!\n") in
        (GameState.find_item_by_id game id).points
      else
        0
    in
    let updated_player =
      {added_item_player with
        room = updated_room;
        score = added_item_player.score - score_to_subtract;
        turns = added_item_player.turns + 1
      } in
    (updated_player, GameState.update_room game updated_room)
  else
    let () = print_string("The room does not have that item\n") in
    (player, game)
(* Drop function for a player to drop an item into a room *)
let drop (game: GameState.params) (player: Player.params) (id: string)
  : Player.params * GameState.params =
  let bool_val = Player.has_item player id in
  if bool_val then
    let lost_item_player = Player.drop_item player id in
    let updated_room = Room.drop_item player.room id in
    let score_to_add =
      if Room.is_treasure player.room id
      then
        let () = print_string ("You dropped a treasure!\n") in
        (GameState.find_item_by_id game id).points
      else
        let () = print_string ("You didn't drop a treasure!\n") in
        0
    in
    let updated_player =
      {lost_item_player with
        room = updated_room;
        score = lost_item_player.score + score_to_add;
        turns = lost_item_player.turns + 1
      } in
    (updated_player, GameState.update_room game updated_room)
    else
      let () = print_string("You dont have that item!\n") in
      (player, game)
(* Inventory function for a player that returns the inventory of the items the player has *)
let inventory (player: Player.params) : unit =
  print_string("Your inventory is: \n");print_item_list player.inventory;print_string("\n")
(* Quit function for a player to end the game *)
let quit (game: GameState.params) : GameState.params =
  GameState.end_game game
(* Score function for a player that returns the score of the player *)
let score (player: Player.params) : unit =
  print_string("Your score is: \n");print_int (player.score);print_string("\n")
(* Turns function for a player that returns the number of turns that the player has had *)
let turns (player: Player.params) : unit =
  print_string("Your turns are: \n");print_int (player.turns);print_string("\n")
let is_valid_go (player: Player.params) (game: GameState.params)
(direction: string) : Player.params * GameState.params =
(* Recursive repl that is going to be used to act as an infinite while loop *)


let rec repl (game: GameState.params) (player: Player.params) : unit =
  if ((player.score = game.total_score) || (game.game_over)) then
    print_string("Hope you enjoyed the adventure!")
  else
    let () = print_string("What now?: ") in
    let input = String.lowercase(String.trim(read_line())) in
    let input_array = Str.split (Str.regexp "[ \t]+") input in
    match input_array with
    | ["go";direction;] ->
      let () = print_string("go") in
      repl game player
    | ["take";obj;] ->
      let () = print_string("take") in
      repl game player
    | ["drop";obj;] ->
      let () = print_string("drop") in
      repl game player
    | ["inventory";] | ["inv";] ->
      let () = inventory player in
      repl game player
    | ["look";] ->
      let () = look player in
      repl game player
    | ["score";] ->
      let () = score player in
      repl game player
    | ["turns";] ->
      let () = turns player in
      repl game player
    | ["quit";] ->
      repl ({ game with game_over = true}) (player)
    | [direction] ->
      let () = print_string("go") in
      repl game player
    | _ ->
      let () = print_string("That is not a valid function\n") in
      repl game player


(* Testing functions *)
let file_input = String.lowercase(String.trim(Sys.argv.(1)))
let data = Yojson.Basic.from_file file_input
let rooms = List.map Room.create (data|> member "rooms" |> to_list)
let items = List.map Item.create (data|> member "items" |> to_list)
let start_items_string = List.map to_string (data |> member "start_items" |> to_list)
let game = GameState.create items rooms
let start_items = List.nth (List.map (fun (item_string: string) ->
  List.filter (fun (item: Item.params) -> (item.id = item_string)) game.all_items
) start_items_string) 0
let start_room = GameState.find_room_by_id game (data |> member "start_room" |> to_string)
let player = Player.create start_items start_room 0 0 game
let () = repl game player￵