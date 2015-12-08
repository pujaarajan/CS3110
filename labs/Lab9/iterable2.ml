(* Queue, ListQueue, and TwoListQueue are essentially the same as
   the code from the previous lecture on modules.  But we've now
   added a fold function (and no iter function). *)

module type Queue = sig
  type 'a queue
  exception Empty
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a
  val dequeue : 'a queue -> 'a queue
  val fold : 'a queue -> 'acc -> ('a -> 'acc -> 'acc) -> 'acc
end

module ListQueue : Queue = struct
  type 'a queue = 'a list
  exception Empty
  let empty = []
  let is_empty q = q = []
  let enqueue x q = q @ [x] 
  let peek = function
    | [] -> raise Empty
    | x::_ -> x
  let dequeue = function
    | [] -> raise Empty
    | _::q -> q
  let fold q acc f = List.fold_right f q acc    
end

module TwoListQueue : Queue = struct
  type 'a queue = {front:'a list; back:'a list}
  exception Empty 
  let empty = {front=[]; back=[]}
  let is_empty = function
    | {front=[]; back=[]} -> true
    | _ -> false
  let norm = function
    | {front=[]; back} -> {front=List.rev back; back=[]}
    | q -> q
  let enqueue x q = norm {q with back=x::q.back}
  let peek = function 
    | {front=[]; _} -> raise Empty
    | {front=x::_; _} -> x
  let dequeue = function
    | {front=[]; _} -> raise Empty
    | {front=_::xs; back} -> norm {front=xs; back}
  let fold {front;back} acc f =
    let mid = List.fold_right f front acc in
    List.fold_right f (List.rev back) mid
end

(* A data structure that is foldable must be represented by a type
   'a t and have a fold function that operates on that type. *)
module type Foldable = sig
  type 'a t
  val fold : 'a t -> 'acc -> ('a -> 'acc -> 'acc) -> 'acc
end

(* A data structure that is iterable must be represented by a type
   'a t and have an iter function that operates on that type. *)
module type Iterable = sig
  type 'a t
  val iter : 'a t -> ('a -> unit) -> unit
end

(* The MakeIterable functor takes in a foldable structure and returns
   an iterable structure. *)
module MakeIterable (F:Foldable) = struct
  type 'a t = 'a F.t
  let iter x f = F.fold x () (fun b () -> f b)
end

(* At this point it might seem like we could write the following code 
   to get iterable queues.  But if you uncomment it, you'll get an error: 
     The type `t' is required but not provided. *)
     
(* module IterableListQueue = MakeIterable(ListQueue) *)

(* That's because MakeIterable requires a structure that conforms to the
   Foldable signature, and ListQueue technically does *not* conform
   to it:  Foldable requires a type 'a t, but ListQueue does not have
   a type by that name.  ListQueue does, however, have a type 'a queue,
   and that is the type that ListQueue.fold works with.  So we need
   to add the type 'a t to ListQueue, and record the fact that it's the
   same type as 'a queue.  We can write a functor QueueWithT that does that:
*)   

module QueueWithT (Q:Queue) = struct
  include Q
  type 'a t = 'a Q.queue
end

(* And now we could apply the MakeIterable functor: *)

module IterableListQueue' = MakeIterable(QueueWithT(ListQueue))

(* However, the resulting module IterableListQueue' isn't very useful. 
   The only operation contained in the structure returned by MakeIterable
   is iter.  Nothing else from the Queue signature is there.  So we 
   can't write code that uses IterableListQueue'.  For example,
   try uncommenting the next line.  You'll get an error:  
     Error: Unbound value enqueue. 
   That's because enqueue is not actually provided by IterableListQueue'. *)

(* let q1 = IterableListQueue'.(enqueue 5 empty) *)

(* To solve that problem, we can write another functor that takes in
   any Queue and returns a structure with all the operations from
   that Queue **as well as** the iter operation. *)

module MakeIterableQueue (Q:Queue) = struct
  include Q
  include MakeIterable(QueueWithT(Q))
end

(* And that functor suffices to do what we wanted:  get an iter function
   for free out of a data structure that supplies a fold function *)

module IterableListQueue    = MakeIterableQueue(ListQueue)
module IterableTwoListQueue = MakeIterableQueue(TwoListQueue)

let q1 = IterableListQueue.(enqueue 5 empty)
let _  = IterableListQueue.iter q1 print_int

let q2 = IterableTwoListQueue.(enqueue 5 empty)
let _  = IterableTwoListQueue.iter q2 print_int
    
