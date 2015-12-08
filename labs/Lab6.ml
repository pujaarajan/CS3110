let rec list_max_1 (l: 'a list)  =
  match l with
  | []     -> failwith "message"
  | hd::tl -> max hd::(list_max_1 tl)


(* write a function safe_hd : 'a list -> 'a option that returns Some x if the
head of the input list is x, and None if the input list is empty. *)

let safe_hd (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | h::t -> Some h

(* write a function safe_tl : 'a list -> 'a list option that returns the tail of
the list, or None if the list is empty. *)

let safe_tl (l : 'a list) : 'a list option =
  match l with
  | [] -> None
  | h::t -> Some t

(* write a function list_max : 'a list -> 'a option that returns the maximum
element of a list, or None if the list is empty.*)

(*HELP*)

let list_max_2 (l:'a list) : 'a option =
  match l with
  | []     -> None
  | hd::tl -> Some (max hd::tl)

(*Define the type pokemon to be a record with fields name (a string), hp (an
integer), and ptype (as defined in lecture).*)

type pokemon = {name : string; hp : int; ptype : ptype}
type ptype = TNormal | TFire | TWater


(*Create a record named charizard of type pokemon that represents a Pokémon with
 78 HP and Fire type.*)

utop # { name = "charizard"; hp = 78; ptype = TFire};;
- : pokemon = {name = "charizard"; hp = 78; ptype = TFire}

(*Create a record named metapod of type pokemon that represents a Pokémon with
50 HP and Normal type.*)

utop # { name = "metapod"; hp = 50; ptype = TNormal};;
- : pokemon = {name = "metapod"; hp = 50; ptype = TNormal}

(*Write a function max_hp : pokemon list -> pokemon option that, given a list of
pokemon, finds the Pokémon with the highest HP.*)

(*HELP*)

let max_hp (pl : pokemon list) : pokemon option =
  List.fold_left (fun acc x y -> if y.hp > acc then acc = x.hp and x = y.name else acc) 0 "" pl

(*Write a function avg_hp : pokemon list -> float option that, given a list of 
pokemon, finds the average HP of Pokémon in the list. If the list is empty, it
should return None.*)

(*HELP*)

let avg_hp (pl : pokemon list) : float option =
List.fold_left (fun acc x -> (x.hp + acc)/2) 0 pl



(*Write a function is_before that takes two dates as input and evaluates to true
or false. It evaluates to true if the first argument is a date that comes
before the second argument. (If the two dates are the same, the result is 
false.)*)

let is_before (d1: int*int*int) (d2: int*int*int) =
  match d1 with
  | (a,b,c) ->
    match d2 with
    | (d,e,f) -> if d>a then true
                 else
                 if d = a && e > b then true
                 else
                 if d = a && e = b && f > c then true
                 else false


(*Write a function string_of_date that takes a date, and returns a string
representing that date in middle endian format with the month name spelled
out. For example, the date (2011,4,22) would be represented as "April 22, 2011".*)

let string_of_date (d: int*int*int) =
  match d with
  | (a,b,c) -> print_string

  (*HELP HOW TO PRINT STRINGS WITH NUMBERS*)


(*Write a function earliest : (int*int*int) list -> (int*int*int) option. It
evaluates to None if the input list is empty, and to Some d if date d is the
earliest date in the list.*)

let earliest (d: (int*int*int) list) : (int*int*int) option  =

(*HELP*)



(*earliest and max_hp (above) have a similar specification. Write a higher-order
 helper function and use it to reimplement them both.*)

(*HELP*)