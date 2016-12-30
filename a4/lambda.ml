(*** An extended example of a lambda calculus interpreter written in OCalf ***)

open Ast
open TypedAst
open Printer

(** apply f [e1; e2; e3] is App(App(App(f, e1), e2), e3) *)
let apply = List.fold_left (fun f e -> App (f,e))

let lambda_spec = {
  vars = [];
  name = "expr";
  constructors = [
    "Lambda", TStar (TString, TVariant ([], "expr"));
    "App",    TStar (TVariant ([], "expr"), TVariant ([], "expr"));
    "Var",    TString;
    "Int",    TInt;
  ];
}

let lambda_step_spec = {
  vars = [];
  name = "step_result";
  constructors = [
    "Value",    TVariant ([], "expr");
    "Stuck",    TUnit;
    "Progress", TVariant ([], "expr");
  ];
}

let lambda_subst_body =
  Fun("e", Fun("v", Fun ("x", Match (Var "e", [
      PVariant ("Lambda", PPair (PVar "y", PVar "e")),
	If (BinOp (Eq, Var "x", Var "y"),
	    Variant ("Lambda", Pair (Var "y", Var "e")),
	    Variant ("Lambda", Pair (Var "y",
				     apply (Var "subst") [Var "e"; Var "v"; Var "x"])))
      ;
      PVariant ("App", PPair (PVar "e1", PVar "e2")),
	Variant ("App", Pair  (apply (Var "subst") [Var "e1"; Var "v"; Var "x"],
			       apply (Var "subst") [Var "e2"; Var "v"; Var "x"]))
      ;
      PVariant ("Var", PVar "y"),
	If (BinOp (Eq, Var "x", Var "y"),
	    Var "v",
	    Variant ("Var", Var "y"))
      ;
      PVariant ("Int", PVar "n"),
	Variant ("Int", Var "n")
    ]))))

let lambda_subst = LetRec ("subst", lambda_subst_body, Var "subst")

let lambda_step_body = Fun("e", Match (Var "e", [
    PVariant ("Lambda", PVar "_"), Variant ("Value", Var "e");
    PVariant ("Int",    PVar "_"), Variant ("Value", Var "e");
    PVariant ("App", PPair (PVariant ("Lambda", PPair (PVar "x", PVar "e1")),
			    PVar "e2")),
      Match (App (Var "step", Var "e2"), [
	PVariant ("Value",    PVar "v"),
	  Variant ("Progress", apply (Var "subst") [Var "e1"; Var "v"; Var "x"]);
	PVariant ("Stuck",    PUnit),
	  Variant ("Stuck", Unit);
	PVariant ("Progress", PVar "e2'"),
	  Variant ("Progress", Variant ("App", Pair (Variant ("Lambda", Pair (Var "x", Var "e1")),
						    Var "e2'")));
      ]);
    PVariant ("App", PPair (PVar "e1", PVar "e2")),
      Match (App (Var "step", Var "e1"), [
	PVariant ("Progress", PVar "e1'"),
	  Variant ("Progress", Variant ("App", Pair (Var "e1'", Var "e2")));
	PVar "_",
	  Variant ("Stuck", Unit);
      ]);
    PVar "_",
      Variant ("Stuck", Unit);
    ];
  ))

let lambda_step =
  LetRec ("subst", lambda_subst_body,
  LetRec ("step",  lambda_step_body,
  Var "step"))

let lambda_run_body =
  Fun ("e", Match (App (Var "step", Var "e"), [
    PVariant ("Progress", PVar "e'"),
      App (Var "run", Var "e'");
    PVar "res",
      Var "res";
  ]))

let lambda_run = 
  LetRec ("subst", lambda_subst_body,
  LetRec ("step",  lambda_step_body,
  LetRec ("run",   lambda_run_body,
  Var "run")))

let lambda_expr =
  Variant ("App", Pair (Variant ("App", Pair (
    Variant ("Lambda", Pair (String "x",
      Variant ("Lambda", Pair (String "y",
        Variant ("Var", String "x"))))),
    Variant ("Int", Int 0))), Variant ("Int", Int 1)))

let lambda_zero =
  Variant ("Lambda", Pair (String "f", Variant ("App", Pair (Variant ("Var", String "x"), Variant ("Int", Int 0)))))

let lambda_succ =
  Variant ("Lambda", Pair (String "n", Variant ("Lambda", Pair (String "f",
    Variant ("App", Pair (Variant ("Var", String "f"),
                          Variant ("App", Pair (Variant ("Var", String "n"), 
                                                Variant ("Var", String "f")))))))))

let lambda_test =
  LetRec ("subst", lambda_subst_body,
  LetRec ("step",  lambda_step_body,
  LetRec ("run",   lambda_run_body,
  Let    ("zero",  lambda_zero,
  Let    ("succ",  lambda_succ,
  App (Var "run", lambda_expr))))))

