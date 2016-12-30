{
open Parser
exception Eof

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let ident      = (['a'-'z'] | '_') (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
let constr     = ['A'-'Z'] (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
let integral   = ['0'-'9']+
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf } (* skip blanks *)
  | ['\n'] { incr_linenum lexbuf; token lexbuf }
  | "fun"  { FUN }
  | "false" { FALSE }
  | "true" { TRUE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { PIPE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | ">" { GT }
  | "<" { LT }
  | "<=" { LTQ }
  | ">=" { GTQ }
  | "=" { EQ }
  | "<>" { NEQ }
  | "^" { CONCAT }
  | "rec" { REC }
  | "let" { LET }
  | "in" { IN } 
  | "()" { UNIT }
  | ","  { COMMA }
  | '(' | "begin"    { LPAREN }
  | ')' | "end"   { RPAREN }
  | "->"   { ARROW }
  | "type" { TYPE }
  | "of" { OF }
  | "'" (ident as id) { TVAR id }
  | "unit" { TUNIT }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "string" { TSTRING }
  | ident as id { VAR id }
  | constr as c { CONSTR c }
  | integral as i {INT i}
  | '"' ([^'"']* as str) '"' { STRING str }
  | eof { EOF }

