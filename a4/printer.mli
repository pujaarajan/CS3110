
(** This module contains functions for nicely printing things of various types.
 *
 * It contains three kinds of functions:
 *
 *   - print_foo : foo -> unit
 *       prints a value of type foo to standard output.  These are useful for
 *       working at the toplevel.
 *
 *   - string_of_foo : foo -> string
 *       converts a value of type foo to a string
 *
 *   - format_foo : Format.formatter -> foo -> unit
 *       these functions are more general.  They can be used with the Format
 *       module's printf functions to print values of type foo.  For example,
 *
 *         Format.printf "%a evaluates to %a" format_expr e format_value v
 *
 *       will print something like "3+5 evaluates to 8"
 *
 *       The format_foo function can also be installed in utop by typing
 *         #install_printer format_foo;;
 *       This will cause utop to call format_foo whenever it prints out a value
 *       of type foo.  For example:
 *
 *         utop# e;;
 *         val - : expr = BinOp(Plus (Int 3, Int 5))
 *
 *         utop# #install_printer format_expr;;
 *         utop# e;;
 *         val - : expr = 3 + 5
 *)

val print_expr     : Ast.expr      -> unit
val print_operator : Ast.operator  -> unit
val print_pattern  : Ast.pattern   -> unit

val print_type     : TypedAst.typ               -> unit
val print_var_spec : TypedAst.variant_spec      -> unit
val print_aexpr    : TypedAst.annotated_expr    -> unit
val print_apat     : TypedAst.annotated_pattern -> unit

val make_printer : (Format.formatter -> 'a -> unit) -> 'a -> unit

(* functions for converting things from various types to strings *)

val string_of_expr     : Ast.expr     -> string
val string_of_operator : Ast.operator -> string
val string_of_pattern  : Ast.pattern  -> string

val string_of_type     : TypedAst.typ               -> string
val string_of_var_spec : TypedAst.variant_spec      -> string
val string_of_aexpr    : TypedAst.annotated_expr    -> string
val string_of_apat     : TypedAst.annotated_pattern -> string

val make_string_of : (Format.formatter -> 'a -> unit) -> 'a -> string

(* formatting functions *)

val format_expr      : Format.formatter -> Ast.expr     -> unit
val format_operator  : Format.formatter -> Ast.operator -> unit
val format_pattern   : Format.formatter -> Ast.pattern  -> unit

val format_type      : Format.formatter -> TypedAst.typ               -> unit
val format_var_spec  : Format.formatter -> TypedAst.variant_spec      -> unit
val format_aexpr     : Format.formatter -> TypedAst.annotated_expr    -> unit
val format_apat      : Format.formatter -> TypedAst.annotated_pattern -> unit


