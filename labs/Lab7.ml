
(* Give a type definition and a value of that type for a set of sets, where the
type of elements can by anything, and where sets are represented by lists. *)

type set_of_sets = 'a list list (*WHY DOESN'T THIS WORK*)

(* Give a type definition and a value of that type for a list of triples, the
first two components of which have the same type, and the third component of
which is of some possibly different type. *)

type triples = ('a * 'a * 'b) list (*HOW DO ALPHA WORK*)

(* Give a type definition and a value of that type for the suit of a card deck.
*)

type suit = Heart | Club | Spade | Diamond


(* Give a type definition and a value of that type for a type whose values are
"things", where a "thing" is either an integer or a list of "things". *)

type things = Thing1 of int | Thing2 of things list

(* Give a type definition and a value of that type for a type whose values are
pairs whose components can be of any type, as long as they are of the same type.
*)

type pairs = Pair of ('a * 'a) (*HELP HOW OT DO ALPHA*)


(*A binary search tree is made up of nodes. Each node is either a data node,
which contains a key, a value, a left child, and a right child, or it is a leaf
node, which contains nothing at all. Complete the following type definition. *)

type ('k, 'v) bstree = DataNode of (key, value, left, right) | Leaf

(*HELP*)