(*** Using functor to test queues ***)

(* We start by giving our usual two implementations of queues. *)

module type Queue = sig
  type 'a queue
  exception Empty
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a
  val dequeue : 'a queue -> 'a queue
end

module ListQueue : Queue = struct
  type 'a queue = 'a list
  exception Empty
  let empty = []
  let is_empty q = q = []
  let enqueue x q = q @ [x] (* linear-time enqueue :( *)
  let peek = function
    | [] -> raise Empty
    | x::_ -> x
  let dequeue = function
    | [] -> raise Empty
    | _::q -> q
end

module TwoListQueue : Queue = struct
  (* {front=[a;b]; back=[e;d;c]} represents the queue
   * containing the elements a,b,c,d,e. That is, the
   * back of the queue is stored in reverse order. *)
  type 'a queue = {front:'a list; back:'a list}
  exception Empty 
  let empty = {front=[]; back=[]}
  let is_empty = function
    | {front=[]; back=[]} -> true
    | _ -> false
  (* a queue is in normal form if the front being empty
   * implies the entire queue is empty *)
  let norm = function
    | {front=[]; back} -> {front=List.rev back; back=[]}
    | q -> q
  let enqueue x q = norm {q with back=x::q.back} (* constant-time enqueue :) *)
  let peek = function 
    | {front=[]; _} -> raise Empty
    | {front=x::_; _} -> x
  let dequeue = function
    | {front=[]; _} -> raise Empty
    | {front=_::xs; back} -> norm {front=xs; back}
end

(* Now we write a functor that can test an arbitrary implementation of Queue.
   The test it runs is to create a long list, enqueue all the elements of it,
   then dequeue all the elements into a new list, and check to see that
   the original list and the new list are the same. Of course, we could
   add additional tests to the functor.  *)

module QueueTester (Q: Queue) = struct
  let from_list = List.fold_left (fun q x -> Q.enqueue x q) Q.empty
      
  let rec to_list q =
    let rec to_revlist q acc =
      if Q.is_empty q then acc
      else to_revlist (Q.dequeue q) ((Q.peek q)::acc) in
    List.rev(to_revlist q [])

  let (--) i j =
    let rec from i j l =
      if i>j then l
      else from (i+1) j (i::l)
    in from i j []
      
  let long_list = 0 -- 100_000

  let run_test () = assert (to_list(from_list(long_list)) = long_list)
end

module ListQueueTester    = QueueTester(ListQueue)
module TwoListQueueTester = QueueTester(TwoListQueue)

(* The next line is commented out because the test takes too long to run
   for long lists!  Try reducing the length of long_list, above, to make
   the test run faster. *)
(* let () = ListQueueTester.run_test() *) 

let () = TwoListQueueTester.run_test()
