open Ast
open TypedAst
open Printer
open Parser
open Infer
open Eval
open Lambda
open Assertions

(*** Examples of [eval] ***)

let expr_example  = Parse.parse_expr "fun x -> 3 + x"
let expr_example' = Fun ("x", BinOp (Plus, Int 3, Var "x"))

TEST_UNIT = expr_example === expr_example'

let eval_example  = Parse.parse_expr "if false then 3 + 5 else 3 * 5"
let eval_example' = If (Bool false, BinOp (Plus, Int 3, Int 5), BinOp (Times, Int 3, Int 5))

TEST_UNIT = eval_example === eval_example'
TEST_UNIT = eval [] eval_example === VInt 15

(*** Examples of [infer] ***)

let infer_example  = Parse.parse_expr "fun x -> 3 + x"
let infer_example' = Fun ("x", BinOp(Plus, Int 3, Var "x"))

TEST_UNIT = infer_example === infer_example'

let infer_example_type  = Parse.parse_type "int -> int"
let infer_example_type' = TArrow (TInt,TInt)

TEST_UNIT = infer_example_type === infer_example_type'

TEST_UNIT = typeof (infer [] infer_example) === infer_example_type


let option_spec  = Parse.parse_variant_spec "type 'a option = Some of 'a | None of unit"
let option_spec' = {
  vars = ["a"];
  name = "option";
  constructors = [
    "Some", TAlpha "a";
    "None", TUnit;
  ];
}

TEST_UNIT = option_spec === option_spec'


let list_spec  = Parse.parse_variant_spec "type 'a list = Nil of unit | Cons of ('a * 'a list)"
let list_spec' = {
  vars = ["a"];
  name = "list";
  constructors = [
    "Nil",  TUnit;
    "Cons", TStar (TAlpha "a", TVariant ([TAlpha "a"], "list"));
  ];
}

TEST_UNIT = list_spec === list_spec'


let infer_variant  = Parse.parse_expr "(Some 1, Some \"where\")"
let infer_variant' = Pair (Variant ("Some", Int 1),
                           Variant ("Some", String "where"))

TEST_UNIT = infer_variant === infer_variant'


let infer_variant_type  = Parse.parse_type "int option * string option"
let infer_variant_type' = TStar (TVariant ([TInt], "option"), TVariant ([TString], "option"))

TEST_UNIT = infer_variant_type === infer_variant_type'

TEST_UNIT = typeof (infer [option_spec] infer_variant) === infer_variant_type


(*** other examples ***)

let map  = Parse.parse_expr "let rec map = fun f -> fun l -> match l with
                                | Nil ()       -> Nil ()
                                | Cons (hd,tl) -> Cons (f hd, map f tl)
                              in map"
let map' = LetRec ("map", Fun ("f", Fun ("l",
                            Match (Var "l", [
                              PVariant ("Nil",  PUnit),
                                Variant ("Nil", Unit);
                              PVariant ("Cons", PPair(PVar "hd", PVar "tl")),
                                Variant ("Cons", Pair (App (Var "f", Var "hd")
                                                      ,App (App (Var "map", Var "f"), Var "tl")
                                                      ));
                            ]))),
                   Var "map")

TEST_UNIT = map === map'

let map_type  = Parse.parse_type "('a -> 'b) -> 'a list -> 'b list"
let map_type' = TArrow(TArrow(TAlpha "a", TAlpha "b"),
                       TArrow(TVariant ([TAlpha "a"], "list"),
                              TVariant ([TAlpha "b"], "list")))


TEST_UNIT = map_type === map_type'

TEST_UNIT = typeof (infer [list_spec] map) === map_type

let fold  = Parse.parse_expr "let rec fold = fun f -> fun l -> fun a -> match l with
                                 | Nil () -> a
                                 | Cons (hd,tl) -> f hd (fold f tl a)
                               in fold"
let fold' = LetRec ("fold", Fun ("f", Fun ("l", Fun ("a", Match (Var "l", [
              PVariant ("Nil", PUnit),
                Var "a";
              PVariant ("Cons", PPair (PVar "hd", PVar "tl")),
                App (App (Var "f", Var "hd"), App (App (App (Var "fold", Var "f"), Var "tl"), Var "a"))
           ])))), Var "fold")

TEST_UNIT = fold === fold'

let fold_type = Parse.parse_type "('a -> 'b -> 'b) -> 'a list -> 'b -> 'b"

TEST_UNIT = typeof (infer [list_spec] fold) === fold_type


(*** An example for the let-polymorphism karma problem ***)
(* Uncomment the code below if you're working on that karma problem. *)
(*
let infer_poly  = Parse.parse_expr "let any = fun x -> x in (any 1, any \"where\")"
let infer_poly' = Let ("any", Fun ("x", Var "x"),
                      Pair (App (Var "any", Int 1),
                            App (Var "any", String "where")))

TEST_UNIT = infer_poly === infer_poly'

let infer_poly_type  = Parse.parse_type "int * string"
let infer_poly_type' = TStar (TInt, TString)

TEST_UNIT = infer_poly_type === infer_poly_type'

TEST_UNIT = typeof (infer [] infer_poly) === infer_poly_type
*)