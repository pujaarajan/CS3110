(*** Generic matrix arithmetic ***)

(* A Ring is an algebraic structure that generalizes the notion of
   addition and multiplication. *)
module type Ring = sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val mult : t -> t -> t
end

(* We can create Rings over many types:  ints, floats, bools, ... *)

module IntRing = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let mult = ( * )
end

module FloatRing = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let add = (+.)
  let mult = ( *. )
end

module BoolRing = struct
  type t = bool
  let zero = false
  let one = true
  let add = (||)
  let mult = (&&)
end

let one_plus_one_int   = IntRing.  (add one one)
let one_plus_one_float = FloatRing.(add one one)
let one_plus_one_bool  = BoolRing. (add one one)

(* From an arbitrary Ring, we can produce a Matrix structure. *)

module Matrix (R:Ring) = struct
  let add a b = 
    let add_vectors v1 v2 = List.map2 (R.add) v1 v2 in
    List.map2 add_vectors a b
  let mult a b = failwith "recitation 5 exercise"
end

module IntMatrix   = Matrix(IntRing)
module FloatMatrix = Matrix(FloatRing)
module BoolMatrix  = Matrix(BoolRing)

let im1 = [[1;2;3];[4;5;6]]
let im2 = IntMatrix.add im1 im1

(* However, that implementation of Matrix exposes the fact that it
   represents a matrix as a list of lists.  From a software engineering
   perspective, it would likely be preferable to hide that representation.
   Then the implementer would one day be free to change it if needed. 
   So, let's use a signature to hide the representation type.  We'll
   also include some convenience functions for converting to and from
   lists in row-major representation. *)
   
module type AbstractMatrix = sig
  type elt  (* the type of elements of the matrix *)
  type t    (* the type of the matrix itself *)
  val from_list : elt list list -> t
  val to_list : t -> elt list list
  val add : t -> t -> t
  val mult : t -> t -> t
end

module MakeMatrix (R:Ring) : AbstractMatrix = struct
  type elt = R.t
  type t = elt list list
  let from_list l = l
  let to_list m = m
  let add_vectors v1 v2 = List.map2 (R.add) v1 v2
  let add a b = List.map2 add_vectors a b
  let mult a b = failwith "recitation 5 exercise"
end

module IntMatrix = MakeMatrix(IntRing)

(* But the following client code doesn't compile.  Uncomment it, and
   you will get an error:
     This expression has type int but an expression was expected of type
         IntMatrix.elt. *)
         
(* let im1 = IntMatrix.from_list [[1;2;3];[4;5;6]] *)

(* The problem is that the AbstractMatrix signature makes elt abstract.
   So outside the implementation of the structure that results from 
   MakeMatrix(R), no client can see that elt is actually R.t.  For example,
   outside of MakeMatrix(IntRing), no client can see that elt is actually
   IntRing.t, which is int.  We can fix this by adding a sharing constraint
   that reveals to the outside world that elt and R.t are actually the same
   type. *)

module MakeMatrix (R:Ring) : (AbstractMatrix with type elt = R.t) = struct
  type elt = R.t
  type t = elt list list
  let from_list l = l
  let to_list m = m
  let add_vectors v1 v2 = List.map2 (R.add) v1 v2
  let add a b = List.map2 add_vectors a b
  let mult a b = failwith "recitation 5 exercise"
end

module IntMatrix = MakeMatrix(IntRing)
let im1 = IntMatrix.from_list [[1;2;3];[4;5;6]]
let im2 = IntMatrix.add im1 im1
let l   = IntMatrix.to_list im2