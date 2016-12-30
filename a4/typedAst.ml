open Ast

(******************************************************************************)
(** Type definitions (see .mli) ***********************************************)
(******************************************************************************)

type talpha = string
type tname = string

type typ  =
  | TUnit | TInt | TBool | TString
  | TAlpha of talpha
  | TArrow   of typ * typ
  | TStar    of typ * typ
  | TVariant of typ list * tname

type variant_spec = {
  vars: talpha list;
  name: tname;
  constructors: (constructor * typ) list;
}

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

let annotate e = 
  let next_var  = ref 0 in
  let newvar () = next_var := 1 + !next_var;
                  TAlpha (Format.sprintf "t%02i" !next_var) in

  (* annotates the expression using type variables starting from n.  Returns the
   * next type variable and the annotated expression *)
  let rec ann_expr = function
    | Var x            -> AVar      (newvar (), x)
    | App (e1,e2)      -> AApp      (newvar (), ann_expr e1, ann_expr e2)
    | Fun (x,e)        -> AFun      (newvar (), (x,newvar()), ann_expr e)
    | Let (x,e1,e2)    -> ALet      (newvar (), (x,newvar()), ann_expr e1, ann_expr e2)
    | LetRec (x,e1,e2) -> ALetRec   (newvar (), (x,newvar()), ann_expr e1, ann_expr e2)
    | Unit             -> AUnit     (newvar ())
    | Int n            -> AInt      (newvar (), n)
    | Bool b           -> ABool     (newvar (), b)
    | String s         -> AString   (newvar (), s)
    | Variant (c,e)    -> AVariant  (newvar (), c, ann_expr e)
    | Pair (e1,e2)     -> APair     (newvar (), ann_expr e1, ann_expr e2)
    | BinOp (op,e1,e2) -> ABinOp    (newvar (), op, ann_expr e1, ann_expr e2)
    | If (e1,e2,e3)    -> AIf       (newvar (), ann_expr e1, ann_expr e2, ann_expr e3)
    | Match (e, ps)    -> let ann_case (p,e) = ann_pattern p, ann_expr e in
                          AMatch    (newvar (), ann_expr e, List.map ann_case ps)
  and ann_pattern = function
    | PUnit            -> APUnit    (newvar ())
    | PInt n           -> APInt     (newvar (), n)
    | PBool b          -> APBool    (newvar (), b)
    | PString s        -> APString  (newvar (), s)
    | PVar x           -> APVar     (newvar (), x)
    | PVariant (c,p)   -> APVariant (newvar (), c, ann_pattern p)
    | PPair (p1,p2)    -> APPair    (newvar (), ann_pattern p1, ann_pattern p2)

  in
  ann_expr e

let rec strip = function
  | AVar     (_,x)        -> Var x
  | AApp     (_,e1,e2)    -> App (strip e1, strip e2)
  | AFun     (_,x,e)      -> Fun (fst x, strip e)
  | ALet     (_,x,e1,e2)  -> Let (fst x, strip e1, strip e2)
  | ALetRec  (_,x,e1,e2)  -> LetRec (fst x, strip e1, strip e2)
  | AUnit    _            -> Unit
  | AInt     (_,n)        -> Int n
  | ABool    (_,b)        -> Bool b
  | AString  (_,s)        -> String s
  | AVariant (_,c,e)      -> Variant (c,strip e)
  | APair    (_,e1,e2)    -> Pair (strip e1, strip e2)
  | ABinOp   (_,op,e1,e2) -> BinOp (op, strip e1, strip e2)
  | AIf      (_,e1,e2,e3) -> If (strip e1, strip e2, strip e3)
  | AMatch   (_,e,ps)     -> let strip_case (p,e) = (strip_pattern p, strip e) in
                             Match (strip e, List.map strip_case ps)
and strip_pattern = function
  | APUnit _          -> PUnit
  | APInt  (_,n)      -> PInt n
  | APBool (_,b)      -> PBool b
  | APString (_,s)    -> PString s
  | APVar (_,x)       -> PVar x
  | APVariant (_,c,p) -> PVariant (c,strip_pattern p)
  | APPair (_,p1,p2)  -> PPair (strip_pattern p1, strip_pattern p2)

let typeof = function
  | AVar (t,_) | AApp (t,_,_) | AFun (t,_,_) | ALet (t,_,_,_)
  | ALetRec (t,_,_,_) | AUnit t | AInt (t,_) | ABool (t,_) | AString (t,_)
  | AVariant (t,_,_) | APair (t,_,_) | ABinOp (t,_,_,_) | AIf (t,_,_,_)
  | AMatch (t,_,_)

  -> t

let typeof_pat = function
  | APUnit t | APInt (t,_) | APBool (t,_) | APString (t,_) | APVar (t,_)
  | APVariant (t,_,_) | APPair (t,_,_)

  -> t

