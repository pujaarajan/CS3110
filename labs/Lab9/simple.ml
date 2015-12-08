(*** Simple example of functor ***)

module type X = sig val x : int end

(* IncX is a functor.  It takes in a structure and returns
   a structure.  The structure it takes in as an argument
   is named M in the body of the functor.  That structure
   must conform to the X signature.  *)
module IncX (M : X) = struct
  let x = M.x + 1
end

module A = struct let x = 0 end

(* We can apply IncX to A, because A conforms to the X signature.
   That is, A supplies a value named x of type int. Note that 
   A doesn't have to explicitly be annotated as having type X;
   the OCaml compiler can infer that A conforms to X. *)
   
module B = IncX(A)
module C = IncX(B)
  
let _ = assert (B.x = 1)
let _ = assert (C.x = 2)

(* Here is an alternative syntax for functors.  It's an analogue
   to the syntax for anonymous functions. *)
module IncX' = functor (M : X) -> struct
  let x = M.x + 1
end
