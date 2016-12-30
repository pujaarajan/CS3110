open Parser
open Lexer

let parse_expr s =
  expr token (Lexing.from_string s)

let parse_type s =
  typ  token (Lexing.from_string s)

let parse_variant_spec s =
  variant_spec token (Lexing.from_string s)

type filename = string

let parse_expr_from_file f =
  expr token (Lexing.from_channel (open_in f))

let parse_type_from_file f =
  typ token (Lexing.from_channel (open_in f))

let parse_variant_spec_from_file f =
  variant_spec token (Lexing.from_channel (open_in f))