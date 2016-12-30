open Ast
open TypedAst

(**
 * The implementation of this module is loosely based on the paper
 *
 * "Unparsing Expressions with Prefix and Postfix Operators"
 * by Norman Ramsey
 * http://www.cs.tufts.edu/~nr/pubs/unparse-abstract.html
 *
 *)

(* OCaml expression precedences and associativities
 * derived from http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
 *
 * 8: constant
 * 7: application (left)
 * 6: * / %       (left)
 * 5: + -         (left)
 * 4: ^           (right)
 * 3: = < >       (left)
 * 2: ,
 * 1: if          (right)
 * 0: let match fun
 *)

type precedence    = int
type associativity = Left | Right | Unassoc
type opspec        = precedence * associativity

let opspec = function
  | Var _ | Unit | Int _ | Bool _ | String _
      -> 8, Unassoc
  | App _ | Variant _
      -> 7, Left
  | BinOp (Times,_,_)
      -> 6, Left
  | BinOp (Plus,_,_)  | BinOp (Minus,_,_)
      -> 5, Left
  | BinOp (Concat,_,_)
      -> 4, Right
  | BinOp (Gt,_,_)    | BinOp (Lt,_,_)   | BinOp (Eq,_,_)
  | BinOp (GtEq,_,_)  | BinOp (LtEq,_,_) | BinOp (NotEq,_,_)
      -> 3, Left
  | Pair _
      -> 2, Unassoc
  | Fun _ | Let _ | LetRec _
      -> 0, Right
  | If _ | Match _
      -> 1, Right

let opspec_pat = function
  | PUnit | PInt _ | PBool _ | PString _ | PVar _
      -> 8, Unassoc
  | PVariant _
      -> 7, Left
  | PPair _
      -> 2, Unassoc

let format_operator f = function
  | Plus   -> Format.fprintf f "+"
  | Minus  -> Format.fprintf f "-"
  | Times  -> Format.fprintf f "*"
  | Gt     -> Format.fprintf f ">"
  | Lt     -> Format.fprintf f "<"
  | Eq     -> Format.fprintf f "="
  | GtEq   -> Format.fprintf f ">="
  | LtEq   -> Format.fprintf f "<="
  | NotEq  -> Format.fprintf f "<>"
  | Concat -> Format.fprintf f "^"

(** If we have a parse tree
 *
 *            out
 *            / \
 *          inl inr
 *
 * then
 *   [needs_parens out inl Left]  returns true if inl requires parentheses, and
 *   [needs_parens out inr Right] returns true if inr requires parentheses
 *)
let needs_parens (outer_prec,outer_assoc) (inner_prec, inner_assoc) side =
  if      outer_prec > inner_prec then true
  else if outer_prec < inner_prec then false
  else match side, outer_assoc, inner_assoc with
    | Unassoc, _,     _     -> false
    | Left,    Left,  Left  -> false
    | Right,   Right, Right -> false
    | _ -> true

(* we deliberately violate the 80-column restriction here to make the
    parallelism in the definition clearer, hence easier to read *)
let rec format_expr f e =
  let bracket parent side f e =
    if needs_parens (opspec parent) (opspec e) side
    then Format.fprintf f "(%a)" format_expr e
    else Format.fprintf f  "%a"  format_expr e
  in
  match e with
    | Var x            -> Format.fprintf f "%s"
                                           x
    | App (e1, e2)     -> Format.fprintf f "@[%a %a@]"
                                           (bracket e Left) e1
                                           (bracket e Right) e2
    | Fun (x,e')       -> Format.fprintf f "@[<hv 2>@{<keyword>fun@} @{<def>%s@} ->@ %a@]"
                                           x
                                           (bracket e Right) e'
    | Let (x,e1,e2)    -> Format.fprintf f "@[<hv>@{<keyword>let@} @{<def>%s@} =@;<1 2>%a@ @{<keyword>in@}@ %a@]"
                                           x
                                           (bracket e Unassoc) e1
                                           (bracket e Right) e2
    | LetRec (x,e1,e2) -> Format.fprintf f "@[<hv>@{<keyword>let rec@} @{<def>%s@} =@;<1 2>%a@ @{<keyword>in@}@ %a@]"
                                           x
                                           (bracket e Unassoc) e1
                                           (bracket e Right) e2
    | Unit             -> Format.fprintf f "()"
    | Int n            -> Format.fprintf f "%i" n
    | Bool b           -> Format.fprintf f "%b" b
    | String s         -> Format.fprintf f "\"%s\"" s
    | Variant (c,e')   -> Format.fprintf f "%s %a"
                                           c
                                           (bracket e Right) e'
    | Pair (e1,e2)     -> Format.fprintf f "@[%a,@ %a@]"
                                           (bracket e Left) e1
                                           (bracket e Right) e2
    | BinOp (op,e1,e2) -> Format.fprintf f "@[%a@ %a@ %a@]"
                                           (bracket e Left)  e1
                                           format_operator   op
                                           (bracket e Right) e2
    | If (e1,e2,e3)    -> Format.fprintf f "@[<hv>@{<keyword>if@} %a@ @{<keyword>then@}@;<1 2>%a@ @{<keyword>else@}@;<1 2>%a@]"
                                           (bracket e Unassoc) e1
                                           (bracket e Unassoc) e2
                                           (bracket e Right)   e3
    | Match (e', ps)   -> Format.fprintf f "@[<hv 2>@{<keyword>match@} %a @{<keyword>with@}"
                                           (bracket e Unassoc) e';
                          List.iter begin fun (pi,ei) ->
                            Format.fprintf f "@ | %a@;<1 4>-> %a"
                                             format_pattern pi
                                             (bracket e Right) ei
                          end ps;
                          Format.fprintf f "@]";
                          ()
and format_pattern f p =
  let bracket parent side f p =
    if needs_parens (opspec_pat parent) (opspec_pat p) side
    then Format.fprintf f "(%a)" format_pattern p
    else Format.fprintf f  "%a"  format_pattern p
  in
  match p with
    | PUnit             -> Format.fprintf f "()"
    | PInt n            -> Format.fprintf f "%i" n
    | PBool b           -> Format.fprintf f "%b" b
    | PString s         -> Format.fprintf f "\"%s\"" s
    | PVar x            -> Format.fprintf f "@{<def>%s@}" x
    | PVariant (c,p')   -> Format.fprintf f "%s %a"
                                            c
                                            (bracket p Right) p'
    | PPair (p1,p2)     -> Format.fprintf f "@[%a,@ %a@]"
                                            (bracket p Left)  p1
                                            (bracket p Right) p2


(******************************************************************************)
(** Pretty printing types                                                     *)
(******************************************************************************)

(* OCaml type precedences and associativities derived from
 * http://caml.inria.fr/pub/docs/manual-ocaml/types.html#typexpr
 *
 * 4: constant
 * 3: constructor
 * 2: *
 * 1: -> (right assoc)
 *)

let typespec = function
  | TAlpha _ | TUnit | TInt | TBool | TString
    -> 4, Unassoc
  | TVariant _
    -> 3, Unassoc
  | TStar _
    -> 2, Unassoc
  | TArrow _
    -> 1, Right

let rec format_type f t =
  let rec format_tlist f ts = match ts with
    | []    -> ()
    | [t]   -> format_type f t
    | h::tl -> Format.fprintf f "%a, %a"
                                format_type h
                                format_tlist tl
  in
  let bracket parent side f t =
    if needs_parens (typespec parent) (typespec t) side
    then Format.fprintf f "(%a)" format_type t
    else Format.fprintf f  "%a"  format_type t
  in
  match t with
    | TAlpha x          -> Format.fprintf f "'%s" x
    | TUnit             -> Format.fprintf f "unit"
    | TInt              -> Format.fprintf f "int"
    | TBool             -> Format.fprintf f "bool"
    | TString           -> Format.fprintf f "string"
    | TArrow (t1,t2)    -> Format.fprintf f "%a -> %a"
                                            (bracket t Left)  t1
                                            (bracket t Right) t2
    | TStar (t1,t2)     -> Format.fprintf f "%a * %a"
                                            (bracket t Left)  t1
                                            (bracket t Right) t2
    | TVariant ([],n)   -> Format.fprintf f "%s" n
    | TVariant ([t'],n) -> Format.fprintf f "%a %s"
                                            (bracket t Unassoc) t'
                                            n
    | TVariant (ts,n)   -> Format.fprintf f "(%a) %s"
                                            format_tlist ts
                                            n

let format_type f t = Format.fprintf f "@{<type>%a@}" format_type t

let format_var_spec f s =
  let rec format_list f = function
    | []    -> ()
    | [t]   -> Format.fprintf f "'%s" t
    | h::tl -> Format.fprintf f "'%s, %a" h format_list tl
  in

  let format_vars f = function
    | []  -> ()
    | [t] -> Format.fprintf f "'%s" t
    | ts  -> Format.fprintf f "(%a)" format_list ts
  in

  Format.fprintf f "@[<hv 2>@{<keyword>type@} @{<def>%a %s@} ="
                   format_vars s.vars
                   s.name;
  List.iter begin fun (c, t) ->
    Format.fprintf f "@ | %s @{<keyword>of@} %a"
                     c
                     format_type t
  end s.constructors;
  Format.fprintf f "@]";
  ()

(* we deliberately violate the 80-column restriction here to make the
    parallelism in the definition clearer, hence easier to read *)
let rec format_aexpr f = function
  | AVar (t,x)                -> Format.fprintf f "(%s : %a)"
                                                  x
                                                  format_type t
  | AApp (t,e1,e2)            -> Format.fprintf f "(@[@[%a@]@;<1 2>@[%a@]@]@ : %a)"
                                                  format_aexpr e1
                                                  format_aexpr e2
                                                  format_type t
  | AFun (t, (x,tx), e)       -> Format.fprintf f "(@[<hv 2>@{<keyword>fun@} (@{<def>%s@}:%a) ->@ @[%a@]@]@ :%a)"
                                                  x
                                                  format_type tx
                                                  format_aexpr e
                                                  format_type t
  | ALet (t,(x,tx),e1,e2)     -> Format.fprintf f "(@[<hv>@{<keyword>let@} (@{<def>%s@}:%a) =@;<1 2>@[%a@]@ @{<keyword>in@}@;<1 2>@[%a@]@]@ :%a)"
                                                  x
                                                  format_type tx
                                                  format_aexpr e1
                                                  format_aexpr e2
                                                  format_type t
  | ALetRec (t,(x,tx),e1,e2)  -> Format.fprintf f "(@[<hv>@{<keyword>let rec@} (@{<def>%s@}:%a) =@;<1 2>@[%a@]@ @{<keyword>in@}@;<1 2>@[%a@]@]@ :%a)"
                                                  x
                                                  format_type tx
                                                  format_aexpr e1
                                                  format_aexpr e2
                                                  format_type t
  | AUnit   (t)               -> Format.fprintf f "(() : %a)" format_type t
  | AInt    (t,n)             -> Format.fprintf f "(%i : %a)" n format_type t
  | ABool   (t,b)             -> Format.fprintf f "(%b : %a)" b format_type t
  | AString (t,s)             -> Format.fprintf f "(%s : %a)" s format_type t
  | AVariant (t,c,e)          -> Format.fprintf f "(@[<hv 2>%s@ @[%a@]@]@ : %a)"
                                                  c
                                                  format_aexpr e
                                                  format_type t
  | APair (t,e1,e2)           -> Format.fprintf f "(@[<hv>(@[%a@],@;<1 2>@[%a@])@]@ : %a)"
                                                  format_aexpr e1
                                                  format_aexpr e2
                                                  format_type t
  | ABinOp (t,op,e1,e2)       -> Format.fprintf f "(@[<hv 2>@[%a@]@ %a @[%a@]@]@ :%a)"
                                                  format_aexpr e1
                                                  format_operator op
                                                  format_aexpr e2
                                                  format_type t
  | AIf (t,e1,e2,e3)          -> Format.fprintf f "(@[<hv>@{<keyword>if@} @[%a@]@ @{<keyword>then@}@;<1 2>@[%a@]@ @{<keyword>else@}@;<1 2>@[%a@]@]@ :%a)"
                                                  format_aexpr e1
                                                  format_aexpr e2
                                                  format_aexpr e3
                                                  format_type  t
  | AMatch (t,e,ps)           -> Format.fprintf f "(@[<hv 2>@{<keyword>match@} @[%a@] @{<keyword>with@}"
                                                  format_aexpr e;
                                 List.iter begin fun (pi,ei) ->
                                   Format.fprintf f "@ | @[%a@]@;<1 4>-> @[%a@]"
                                                    format_apat pi
                                                    format_aexpr ei
                                 end ps;
                                 Format.fprintf f "@]@ : %a)"
                                                  format_type t

and format_apat f = function
  | APUnit    (t)       -> Format.fprintf f "(() : %a)" format_type t
  | APInt     (t,n)     -> Format.fprintf f "(%i : %a)" n format_type t
  | APBool    (t,b)     -> Format.fprintf f "(%b : %a)" b format_type t
  | APString  (t,s)     -> Format.fprintf f "(\"%s\" : %a)" s format_type t
  | APVar     (t,x)     -> Format.fprintf f "(@{<def>%s@} : %a)" x format_type t
  | APVariant (t,c,p')  -> Format.fprintf f "(@[<hv 2>%s@ @[%a@]@]@ : %a)"
                                            c
                                            format_apat p'
                                            format_type t
  | APPair    (t,p1,p2) -> Format.fprintf f "(@[<hv>(@[%a@],@;<1 2>@[%a@])@]@ : %a)"
                                            format_apat p1
                                            format_apat p2
                                            format_type t


(******************************************************************************)
(** Printers                                                                  *)
(******************************************************************************)

let set_color = function
  | "keyword" -> "\027[38;5;33m"  (* blue   *)
  | "def"     -> "\027[38;5;208m" (* orange *)
  | "type"    -> "\027[38;5;34m"  (* green  *)
  | _         -> "\027[38;5;124m" (* red    *)

let clear_color _ = "\027[38;5;15m"

let print_tags = {
  Format.mark_open_tag   = set_color;
  Format.mark_close_tag  = clear_color;
  Format.print_open_tag  = ignore;
  Format.print_close_tag = ignore;
}

let make_printer formatter e =
  Format.set_margin 80;
  Format.set_formatter_tag_functions print_tags;
  Format.set_tags true;
  Format.printf "@<0>%s" (clear_color ());
  Format.printf "%a@." formatter e

let print_expr     = make_printer format_expr
let print_operator = make_printer format_operator
let print_pattern  = make_printer format_pattern

let print_type     = make_printer format_type
let print_var_spec = make_printer format_var_spec
let print_aexpr    = make_printer format_aexpr
let print_apat     = make_printer format_apat

(******************************************************************************)
(** String conversion                                                         *)
(******************************************************************************)

let make_string_of f = Format.asprintf "%a" f

let string_of_expr     = make_string_of format_expr
let string_of_operator = make_string_of format_operator
let string_of_pattern  = make_string_of format_pattern

let string_of_type     = make_string_of format_type
let string_of_var_spec = make_string_of format_var_spec
let string_of_aexpr    = make_string_of format_aexpr
let string_of_apat     = make_string_of format_apat
