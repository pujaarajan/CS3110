%{
(* This parser is implemented with ocamlyacc, not menhir. *)

open Ast
open TypedAst
open Lexing

let parse_error _ =
  let start_pos = Parsing.symbol_start_pos () in
  let end_pos = Parsing.symbol_end_pos () in
  let start_line = string_of_int start_pos.pos_lnum in
  let start_char = string_of_int (start_pos.pos_cnum - start_pos.pos_bol) in
  let end_line = string_of_int end_pos.pos_lnum in
  let end_char = string_of_int (end_pos.pos_cnum - end_pos.pos_bol) in
  failwith ("Parse error: ("^start_line^"."^start_char^"-"^end_line^"."^end_char)
%}

%token ASSIGN
%token MATCH
%token WITH
%token PIPE
%token FALSE
%token TRUE
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token MULT
%token CONCAT
%token LET
%token REC
%token IN
%token GT
%token LT
%token LTQ
%token GTQ
%token EQ
%token NEQ
%token AND
%token COMMA
%token <string> VAR
%token <string> CONSTR
%token <string> STRING
%token <string> INT
%token LPAREN RPAREN
%token ARROW
%token FUN
%token UNIT
%token EOF
%token TYPE
%token OF
%token <string> TVAR
%token TUNIT
%token TINT
%token TBOOL
%token TSTRING
%token TCONS

%right ARROW
%nonassoc LET
%nonassoc FUN MATCH IF IN
%right PIPE
%nonassoc COMMA
%left GT LT LTQ GTQ EQ NEQ ASSIGN
%right CONCAT
%left PLUS MINUS
%left MULT
%nonassoc VAR LBRACE UNIT INT TRUE FALSE LPAREN STRING
%left CONSTR APP

%left TCONS

/* entry point */

%start expr
%type <Ast.expr> expr

%start typ
%type <TypedAst.typ> typ

%start variant_spec
%type <TypedAst.variant_spec> variant_spec

%start tvar_list
%type <string list> tvar_list

%start constr_list
%type <(string * TypedAst.typ) list> constr_list

%%

expr:
  | UNIT   {Unit}
  | INT    {Int (int_of_string $1)}
  | TRUE   {Bool true}
  | FALSE  {Bool false}
  | STRING {String $1}
  | expr PLUS expr      { BinOp (Plus,   $1, $3) }
  | expr MINUS expr     { BinOp (Minus,  $1, $3) }
  | expr MULT expr      { BinOp (Times,  $1, $3) }
  | expr CONCAT expr    { BinOp (Concat, $1, $3) }
  | expr GT expr        { BinOp (Gt,     $1, $3) }
  | expr LT expr        { BinOp (Lt,     $1, $3) }
  | expr LTQ expr       { BinOp (LtEq,   $1, $3) }
  | expr GTQ expr       { BinOp (GtEq,   $1, $3) }
  | expr EQ expr        { BinOp (Eq,     $1, $3) }
  | expr NEQ expr       { BinOp (NotEq,  $1, $3) }
  | IF expr THEN expr ELSE expr %prec IF
           { If ($2, $4, $6) }
  | VAR    { Var $1 }
  | expr expr %prec APP
           { App ($1, $2) }
  | FUN VAR ARROW expr %prec FUN
           { Fun ($2,$4) }
  | MATCH expr WITH pattern_matching %prec MATCH
           { Match ($2, $4) }
  | MATCH expr WITH PIPE pattern_matching %prec MATCH
           { Match ($2, $5) }
  | LET VAR EQ expr IN expr
           { Let ($2,$4,$6) }
  | LET REC VAR EQ expr IN expr
           { LetRec ($3,$5,$7) }
  | expr COMMA expr %prec COMMA
           { Pair ($1,$3) }
  | CONSTR expr %prec CONSTR
           { Variant ($1,$2) }
  | LPAREN expr RPAREN  %prec LPAREN
           { $2 }
;

pattern_matching:
  | pattern ARROW expr PIPE pattern_matching %prec PIPE
           { ($1, $3)::$5 }
  | pattern ARROW expr
           { [($1, $3)]   }
;

pattern:
  | LPAREN pattern RPAREN {$2}
  | VAR                   {PVar $1}
  | UNIT                  {PUnit}
  | INT                   {PInt (int_of_string $1)}
  | TRUE                  {PBool true}
  | FALSE                 {PBool false}
  | STRING                {PString $1}
  | CONSTR pattern %prec CONSTR
                          {PVariant ($1,$2)}
  | pattern COMMA pattern %prec COMMA
                          {PPair ($1,$3)}
;

typ:
  | TUNIT {TUnit} | TINT {TInt} | TBOOL {TBool} | TSTRING {TString}
  | TVAR  { TAlpha $1 }
  | typ ARROW typ { TArrow ($1,$3) }
  | typ MULT  typ { TStar  ($1,$3) }
  | VAR { TVariant ([], $1) }
  | typ VAR { TVariant ([$1], $2) }
  | LPAREN typ RPAREN { $2 }
  | LPAREN typ_list RPAREN VAR %prec TCONS
        { TVariant ($2, $4) }
;

typ_list:
  | typ {[$1]}
  | typ_list COMMA typ {$1@[$3]}
;

variant_spec:
  | TYPE tvar_list VAR EQ constr_list
       { {vars = $2; name = $3; constructors = $5} }
  | TYPE tvar_list VAR EQ PIPE constr_list
       { {vars = $2; name = $3; constructors = $6} }
;

tvar_list:
  | {[]}
  | TVAR {[$1]}
  | LPAREN tvar_list_inside RPAREN {$2}
;

tvar_list_inside:
  | TVAR {[$1]}
  | TVAR COMMA tvar_list_inside {$1::$3}
;

constr_list:
  | CONSTR OF typ { [($1,$3)] }
  | CONSTR OF typ PIPE constr_list {($1,$3)::$5}
;



