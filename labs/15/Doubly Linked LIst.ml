(* An ['a node] is a node of a mutable doubly-linked list.
 * It contains a value of type ['a] and optionally has 
 * pointers to previous and/or next nodes. *)
type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a
}

(* An ['a dlist] is a mutable doubly-linked list with elements 
 * of type ['a].  It is possible to access the first and 
 * last elements in constant time. *)
type 'a dlist = {
  mutable first : 'a node option;
  mutable last : 'a node option;
}

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
let create_node v = {prev=None; next=None; value=v}

(* [empty_dlist ()] is an empty doubly-linked list. *)
let empty_dlist () = {first=None; last=None}

(* [create_dlist n] is a doubly-linked list containing
 * exactly one node, [n]. *)
let create_dlist n = {first=Some n; last=Some n}

(* [insert_after d n1 n2] mutates dlist [d] by
 * inserting node [n2] after node [n1]. *)
let insert_after (d: 'a dlist) (n1: 'a node) (n2: 'a node) : unit =
  failwith "unimplemented"

(* [iter_forward d f] on a dlist [d] which has 
 * elements n1; n2; ... is (f n1); (f n2); ... *)
let iter_forward (d: 'a dlist) (f: 'a -> unit) : unit =
  failwith "unimplemented"

(* [iter_backward d f] on a dlist [d] which has 
 * elements n1; n2; ... is ...; (f n2); (f n1) *)
let iter_backward (d: 'a dlist) (f: 'a -> unit) : unit =
  failwith "unimplemented"