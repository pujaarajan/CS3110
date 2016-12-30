(*** Abstract syntax of OCalf ***)

(** variable names (e.g. "map" or "x") *)
type var         = string

(** constructor names (e.g. "Some" or "Cons") *)
type constructor = string

(** the binary operators that can be included in OCaml programs. *)
type operator =
  | Plus | Minus | Times                (** +, -, * *)
  | Gt | Lt | Eq | GtEq | LtEq | NotEq  (** >, <, =, >=, <=, <> *)
  | Concat                              (** ^ *)

(** (OCaml) values of type expr represent OCalf expressions. 
    Here are some examples of how expressions are represented:
     - Unit represents () 
     - Int 7 represents 7
     - Bool true represents true
     - String "hello" represents "hello"
     - Binop (Plus, e1, e2) represents e1 + e2
     - If (e1,e2,3) represents if e1 then e2 else e3
     - Var "x" represents the variable x
     - Let ("x",e1,e2) represents let x = e1 in e2
     - LetRec ("x",e1,e2) represents let rec x = e1 in e2
     - App (e1,e2) represents (e1 e2)
     - Fun (x,e) represents (fun x -> e)
     - Pair (e1,e2) represents (e1,e2)
     - Variant ("Cons", e) represents Cons e 
       (where Cons could be any constructor name
     - Match (e, [(p1,e1);(p2,e2);...]) represents
       match e with | p1 -> e1 | p2 -> e1 | ...
     *)
type expr =
  | Unit                               
  | Int      of int 
  | Bool     of bool
  | String   of string
  | BinOp    of operator * expr * expr
  | If       of expr * expr * expr 
  | Var      of var 
  | Let      of var * expr * expr 
  | LetRec   of var * expr * expr 
  | App      of expr * expr  
  | Fun      of var  * expr 
  | Pair     of expr * expr
  | Variant  of constructor * expr
  | Match    of expr * (pattern * expr) list

(** patterns *)
and pattern =
  | PUnit
  | PInt     of int
  | PBool    of bool
  | PString  of string
  | PVar     of var
  | PVariant of constructor * pattern
  | PPair    of pattern * pattern


