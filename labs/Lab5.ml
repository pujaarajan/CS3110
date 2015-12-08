(*Return the product of a list of floats.*)
let product (l: float list) = List.fold_left (fun x acc-> acc *. x) 1 l

(*Find the maximum of a list of floats.*)
let max (l: float list) : float = List.fold_left(fun x acc -> if acc -. x < 0.0 then let acc = x in acc else acc) 0.0 l

(*Add 1.0 to every element of a list of floats.*)
let add_one (l: float list) = List.map (fun x -> x + 1.0) l

(*Find those elements of a list of floats that are greater than 0.0.*)
let greater_than_0 (l:float list) : float list = List.filter (fun x -> x*.(-1.0) < 0.0) l

(*Write each of the following functions in each of three ways:

as a recursive function, without using the List module,

using List.fold_left or List.fold_right, but not other List module functions nor the rec keyword, and

using any combination of List module functions other than fold_left or fold_right, but not the rec keyword.
You will therefore write a total of six functions.*)

(* Write a function lst_and: bool list -> bool, such that lst_and [a1; ...; an]
returns whether all elements of the list are true. That is, it returns a1 && a2
&& ... && an. The lst_and of an empty list is true.*)

let rec lst_and_1 (l: bool list) : bool =
  match l with
  | [] -> true
  | h::t -> h && lst_and_1 t

let lst_and_2 (l: bool list) : bool = List.fold_left (fun acc x -> x&&acc) true l

let lst_and_3 (l:bool list) : bool = if l = [] then true else List.length(List.filter(fun x -> x = false) l) = 0

(* Write a function exists: ('a -> bool) -> 'a list -> bool, such that exists p
[a1; ...; an] returns whether at least one element of the list satisfies the
predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). The exists
of an empty list is false.*)

let rec exists_1 (f: 'a -> bool) (l: 'a list) : bool =
  match l with
  | [] -> false
  | h::t -> f h || exists_1 f t;;

let exists_2 (f: 'a -> bool) (l: 'a list) : bool = List.fold_left (fun acc x -> f x || acc) false  l;;


let exists_3 (f: 'a -> bool) (l: 'a list) : bool = if l = [] then false else List.length(List.filter( fun x -> x = true) (List.map f l)) > 0;;

(*Implement a function is_square: int list list -> bool that returns whether the
 input matrix is square. A matrix is square if the number of rows is equal to the number of columns.*)

let is_square (l : int list list) : bool = List.length(l) = List.length(List.hd(l))

(*Implement a function add_matrices: int list list -> int list list -> int list
list for entry-wise matrix addition. If the two input matrices are not the same
size, the behavior is unspecified.*)

let add_matrices (l1:int list list) (l2:int list list) : int list list=
  if List.length(l1) = List.length(l2) && List.length(List.hd(l1)) = List.length(List.hd(l2))
    then List.map ( (List.map (fun  x y -> x + y) l1) l2)
  else []

(*Implement a function multiply_matrices: int list list -> int list list -> int 
list list that returns the matrix product of the two input matrices. If the two
input matrices are not of sizes that can be multiplied together, the behavior is
 unspecified.*)

let multiply_matrices (l1:int list list) (l2:int list list) : int list list=

(*HELP*)