(*Module type for a query argument*)
module type QUERY_ARG =
sig
  module S : Myset.SET with type elt = Util.CrawlerServices.link
  module D : Dict.DICT with type key = string
                       with type value = S.set
end

(*Query module is a functor that takes an argument of type QUERY_ARG*)
module Query(A : QUERY_ARG) =
struct
  open A

  type query =
      Word of string
    | And of query * query
    | Or of query * query

  (*[parse_words ws] parses a list of strings into a query object,*)
  let rec parse_words ws =
    match ws with
      | w::"AND"::rest -> And(Word w,parse_words rest)
      | w::"OR"::rest -> Or(Word w,parse_words rest)
      | w::[] -> Word w
      | w::rest -> And(Word w,parse_words rest)
      | [] -> raise (Failure "query not understood")

  let query_re = Str.regexp "\\?q=\\(.*\\)"
  let term_sep_re = Str.regexp "\\+"

  (*[parse_query s] splits a string input matching the query_re regular
    expression into a list of strings to be parsed by parse_words*)
  let parse_query s =
    if Str.string_match query_re s 0 then
      let qs = Str.matched_group 1 s in
      let words = Str.split term_sep_re qs
      in
        parse_words words
    else raise (Failure "query not understood")

  (*[eval_query idx q] takes a query q and performs a lookup action in Dict idx
    on each part in the query, creating a set of values subject to the
    structure of the query*)
  let rec eval_query (idx : D.dict) (q:query) : S.set =
    match q with
      | And (l, r) -> S.intersect (eval_query idx l) (eval_query idx r)
      | Or (l, r) -> S.union (eval_query idx l) (eval_query idx r)
      | Word s ->
	match D.lookup idx s with
	  | None -> S.empty
	  | Some s -> s

end
