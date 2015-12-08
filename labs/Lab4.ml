
(*Write a function that returns the product of all the elements in a list. (The product of all the elements of an empty list is 1.)*)
let rec product (l: int list) : int =
  match l with
  | [] -> 1
  | h::t -> h * product t

(*Write a function that concatenates all the strings in a list. (The concatenation of all the strings in an empty list is the empty string "".*)
let rec concatenate (l: string list) : string =
  match l with
  | [] -> ""
  | h::t -> h ^ concatenate t

(*Write a function that returns the last element of a list. Hint: this can be
solved by combining two library functions, and without writing any pattern
matching code of your own. If the list is empty, your function may throw an
exception.*)

let last l = List.hd( List.rev (l))


(*Write a function any_zeroes int list -> bool that returns true if and only
if the input list contains at least one 0. Hint: this can be solved by using one
 library function, and without writing any pattern matching code of your own.*)

let any_zeroes (l: int list) : bool = List.length(List.filter (fun x -> x=0) l) > 0

(*Write a function take : int -> 'a list -> 'a list such that take n lst returns
 the first n elements of lst. If lst has fewer than n elements, return all of
them.*)

let take (i: int) (l: list) : list =
  if i > List.length(l) then l
  else List.fold_left (fun x acc -> if List.length(acc) < i then x::acc else acc) [] l


(*Write a function drop : int -> 'a list -> 'a list such that drop n lst returns
 all but the first n elements of lst. If lst has fewer than n elements, return
the empty list.*)

