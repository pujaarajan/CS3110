open Ast

(******************************************************************************)
(** types *********************************************************************)
(******************************************************************************)

type talpha = string (** a type variable like "a" ('a)  *)
type tname  = string (** a type name like "list" or "option" *)

(** the type of types *)
type typ =
  | TUnit | TInt | TBool | TString
      (** unit, int, bool, and string respectively *)
  | TAlpha of talpha
      (** 'a would be represented as TAlpha "a" *)
  | TArrow   of typ * typ
      (** t1 -> t2 would be represented as TArrow (t1,t2) *)
  | TStar    of typ * typ
      (** t1 * t2 would be represented as TStar (t1,t2) *)
  | TVariant of typ list * tname
      (** 'a list would be represented as TVariant ([TAlpha "a"], "list") *)

(** a record representing all the information about a variant type. 
 * For example, if define the variant ['a list = Nil | Cons of 'a * 'a list]
 * it will be represented as 
 * [{vars = ["a"];
 *   name = "list";
 *   constructors = [("Nil",TUnit); 
 *     ("Cons",TStar(TAlpha "a", TVariant([TAlpha "a"];"list")))]
 *  }]
 *)
type variant_spec = {
  vars: talpha list;
  name: tname;
  constructors: (constructor * typ) list;
}

(******************************************************************************)
(** Annotated expressions                                                    **)
(******************************************************************************)

(**
 * Annotated expressions and patterns are exactly like their unannotated cousins
 * in the Ast module, except that each subexpression has a corresponding type.
 * For example, [AIf (t,ae1,ae2,ae3)]
 * represents [(if e1:t1 then e2:t2 else e3:t3):t] 
 *)
type annotated_expr =
  | AVar      of typ * var
  | AApp      of typ * annotated_expr * annotated_expr
  | AFun      of typ * (var * typ) * annotated_expr
  | ALet      of typ * (var * typ) * annotated_expr * annotated_expr
  | ALetRec   of typ * (var * typ) * annotated_expr * annotated_expr
  | AUnit     of typ
  | AInt      of typ * int
  | ABool     of typ * bool
  | AString   of typ * string
  | AVariant  of typ * constructor * annotated_expr
  | APair     of typ * annotated_expr * annotated_expr
  | ABinOp    of typ * operator * annotated_expr * annotated_expr
  | AIf       of typ * annotated_expr * annotated_expr * annotated_expr
  | AMatch    of typ * annotated_expr * (annotated_pattern * annotated_expr) list
and annotated_pattern =
  | APUnit    of typ
  | APInt     of typ * int
  | APBool    of typ * bool
  | APString  of typ * string
  | APVar     of typ * var
  | APVariant of typ * constructor * annotated_pattern
  | APPair    of typ * annotated_pattern * annotated_pattern



(******************************************************************************)
(** working with annotated expressions                                       **)
(******************************************************************************)

(** extract the type component of an annotated expression *)
val typeof     : annotated_expr -> typ
val typeof_pat : annotated_pattern -> typ

(**
 * Annotate adds a new type variable to each subexpression.
 * The generated type variables are named t01, t02, t03, ...
 *)
val annotate : Ast.expr -> annotated_expr

(** Strip removes all type annotations *)
val strip : annotated_expr -> Ast.expr


