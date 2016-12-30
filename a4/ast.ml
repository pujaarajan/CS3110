type var         = string
type constructor = string

type operator =
  | Plus | Minus | Times                (** +, -, * *)
  | Gt | Lt | Eq | GtEq | LtEq | NotEq  (** >, <, =, >=, <=, <> *)
  | Concat                              (** ^ *)

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
and pattern =
  | PUnit
  | PInt     of int
  | PBool    of bool
  | PString  of string
  | PVar     of var
  | PVariant of constructor * pattern
  | PPair    of pattern * pattern
