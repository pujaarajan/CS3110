
(** This module contains functions for converting strings to ASTs *)

val parse_expr         : string -> Ast.expr
val parse_type         : string -> TypedAst.typ
val parse_variant_spec : string -> TypedAst.variant_spec

type filename = string

val parse_expr_from_file         : filename -> Ast.expr
val parse_type_from_file         : filename -> TypedAst.typ
val parse_variant_spec_from_file : filename -> TypedAst.variant_spec

