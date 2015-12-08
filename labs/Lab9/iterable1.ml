(* Queue, ListQueue, and TwoListQueue are essentially the same as
   the code from the previous lecture on modules.  But we've now
   added a fold and an iter function. *)

module type Queue = sig
  type 'a queue
  exception Empty
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a
  val dequeue : 'a queue -> 'a queue
  val fold : 'a queue -> 'acc -> ('a -> 'acc -> 'acc) -> 'acc
  val iter : 'a queue -> ('a -> unit) -> unit
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
  let iter x f = fold x () (fun b () -> f b)
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
  let iter x f = fold x () (fun b () -> f b)      
end

let q = TwoListQueue.(enqueue 5 empty)
let () = TwoListQueue.iter q print_int
    
(* Note how the implementation of iter is the same in both structures.
   Repeating code like that is bad engineering practice.  We should
   be able to factor it out.  To see how, open iterable2.ml. *)