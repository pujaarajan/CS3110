(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. *)
  val run_tests : unit -> unit
end

(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end

(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)

  let fold f e = List.fold_left (fun a x -> f x a) e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"

  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  (* generates a random list of any size you specify *)
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    let r1 = generate_random_list 100 in
    let s1 = insert_list empty r1 in
    let r2 = generate_random_list 100 in
    let s2 = insert_list empty r2 in
    let u = union s1 s2 in
    let () = List.iter (fun k -> assert(member u k)) r1 in
    let () = List.iter (fun k -> assert(member u k)) r2 in
    assert(not (is_empty u))

  let test_intersect () =
    let r1 = generate_random_list 100 in
    let s1 = insert_list empty r1 in
    let r2 = generate_random_list 100 in
    let s2 = insert_list empty r2 in
    let i = intersect s1 s2 in
    let in_inter k = if (member s1 k)&&(member s2 k)
                     then assert(member i k)
                     else assert(not (member i k)) in
    let () = List.iter in_inter r1 in
    List.iter in_inter r2

  let test_member () =
    let elts = generate_random_list 100 in
    let is_member = member elts (List.hd(elts)) in
    assert (is_member)
    let a = C.gen_random () in
    let b = C.gen_random () in
    let c = C.gen_random () in
    let e = empty in
    let () = assert(not (member e a)) in
    let () = assert(not (member e b)) in
    let () = assert(not (member e c)) in
    let ea = insert a e in
    let () = assert(member ea a) in
    let () = assert(not (member ea b)) in
    let () = assert(not (member ea c)) in
    let eab = insert b ea in
    let () = assert(member eab a) in
    let () = assert(member eab b) in
    let () = assert(not (member eab c)) in
    let eabc = insert c eab in
    let () = assert(member eabc a) in
    let () = assert(member eabc b) in
    assert(member eabc c)

  (*Helper funtion to test choosing by checing whether the key is a member
  * of the dictionary *)
  let rec choosing s  =
    match choose s with
    |None -> assert(is_empty s)
    |Some(k,d)-> let () = assert(member s k) in
                 let () = assert(not (member d k)) in
                 choosing d

  let test_choose () =
    let rands = generate_random_list 100 in
    let inserted = insert_list empty rands in
    choosing inserted

  let print_fold elt base =
    (string_of_elt elt)^" "^base

  let rec string_list lst =
    match lst with
    |[] -> ""
    |h::t -> (string_list t)^(C.string_of_t h)^" "

  let new_compare a b =
  match (C.compare a b) with
  | Eq -> 0
  | Less -> -1
  | Greater -> 1

  let test_fold () =
    let rands = generate_random_list 5 in
    let sorted_rands = List.sort new_compare rands in
    let inserted = insert_list empty rands in
    assert((fold print_fold "" inserted) = string_list sorted_rands)

  let test_is_empty () =
    let emplst = empty in
    assert(is_empty emplst)

  let test_singleton () =
    let k = C.gen () in
    let s = singleton k in
    let () = assert(not (is_empty s)) in
    let () = assert(member s k) in
    let emplst = remove k s in
    assert(is_empty emplst)

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  module D = Dict.Make(struct
      open Order
      type key = C.t
      type value = unit

  let compare x y = C.compare x y
  let string_of_key x = C.string_of_t x
  let string_of_value x = ""
  let gen_key () = C.gen ()
  let gen_key_random () = C.gen_random ()
  let gen_key_gt x () = C.gen_gt x ()
  let gen_key_lt x () = C.gen_lt x ()
  let gen_key_between x y () = C.gen_between x y ()
  let gen_value () = ()
  let gen_pair () = (gen_key (), gen_value ())

  end)

  type elt = D.key
  type set = D.dict

  let empty = D.empty

  let is_empty xs = xs=D.empty

  let insert x xs = D.insert xs x ()

  let singleton x = D.insert D.empty x ()

  let rec union xs ys =
    match D.choose xs with
    |None -> ys
    |Some (k, v, d)-> union d (D.insert ys k ())

  let rec intersect xs ys =
  (*check element is in ys, if so, call insert into intersect*)
    match (D.choose xs) with
    |None -> xs
    |Some (k, v, d) -> if D.member ys k then D.insert (intersect d ys) k ()
                       else intersect d ys

  let remove x xs = D.remove xs x

  let member xs x = D.member xs x

  let choose xs =
    match D.choose xs with
    |None->None
    |Some (k,v,d)->Some(k,d)

  let fold f a set = D.fold (fun  k x a -> f k a) a set

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

(* Inserts elements in a list into a set *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

(* Generates a random list *)
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_empty () =
    let empt = empty in
    assert(is_empty empt)

  let test_singleton () =
    let k = C.gen () in
    let s = singleton k in
    let () = assert(not (D.empty = s)) in
    let () = assert(member s k) in
    let empt = remove k s in
    assert(is_empty empt)

  let test_insert () =
    let rands = generate_random_list 100 in
    let inserted = insert_list empty rands in
    let () = List.iter (fun k -> assert(member inserted k)) rands in
    assert(not (is_empty inserted))

  let test_union () =
    let r1 = generate_random_list 100 in
    let s1 = insert_list empty r1 in
    let r2 = generate_random_list 100 in
    let s2 = insert_list empty r2 in
    let u = union s1 s2 in
    let () = List.iter (fun k -> assert(member u k)) r1 in
    let () = List.iter (fun k -> assert(member u k)) r2 in
    assert(not (is_empty u))

  let test_intersect () =
    let r1 = generate_random_list 100 in
    let s1 = insert_list empty r1 in
    let r2 = generate_random_list 100 in
    let s2 = insert_list empty r2 in
    let i = intersect s1 s2 in
    let in_inter k = if (member s1 k)&&(member s2 k)
                     then assert(member i k)
                     else assert(not (member i k)) in
    let () = List.iter in_inter r1 in
    List.iter in_inter r2

  let test_remove () =
    let rands = generate_random_list 100 in
    let inserted = insert_list empty rands in
    let removed = List.fold_right (fun k r -> remove k r) rands inserted in
    List.iter (fun k -> assert(not (member removed k))) rands

  let test_member () =
    let a = C.gen_random () in
    let b = C.gen_random () in
    let c = C.gen_random () in
    let e = empty in
    let () = assert(not (member e a)) in
    let () = assert(not (member e b)) in
    let () = assert(not (member e c)) in
    let ea = insert a e in
    let () = assert(member ea a) in
    let () = assert(not (member ea b)) in
    let () = assert(not (member ea c)) in
    let eab = insert b ea in
    let () = assert(member eab a) in
    let () = assert(member eab b) in
    let () = assert(not (member eab c)) in
    let eabc = insert c eab in
    let () = assert(member eabc a) in
    let () = assert(member eabc b) in
    assert(member eabc c)

(*Helper function to help test choose by checking wither the key is a member
* in the dictionary or not*)
  let rec choosing s  =
    match choose s with
    |None -> assert(is_empty s)
    |Some(k,d)-> let () = assert(member s k) in
                 let () = assert(not (member d k)) in
                 choosing d

  let test_choose () =
    let rands = generate_random_list 100 in
    let inserted = insert_list empty rands in
    choosing inserted

  let list_fold e l=
    List.append l [e]

  let new_compare a b =
  match (C.compare a b) with
  | Eq -> 0
  | Less -> -1
  | Greater -> 1

  let test_fold () =
    let rands = generate_random_list 100 in
    let inserted = insert_list empty rands in
    let folded = fold list_fold [] inserted in
    let rands_sorted = List.sort_uniq new_compare rands in
    let fold_sorted = List.sort_uniq new_compare folded in
    assert (fold_sorted = rands_sorted)

  let run_tests () =
    test_empty () ;
    test_singleton () ;
    test_insert () ;
    test_union () ;
    test_intersect () ;
    test_remove () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable)
let _ = IntListSet.run_tests()

module IntDictSet = DictSet(IntComparable)
let _ = IntDictSet.run_tests()

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)
(*
module IntDictSet = DictSet(IntComparable)
let _ = IntDictSet.run_tests()
*)

(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  (*ListSet (C)*)
  DictSet (C)
