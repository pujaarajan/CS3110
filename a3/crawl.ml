open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all()


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  (*If frontier is empty, return d*)
  match LinkSet.choose frontier with
  |None -> d
  |Some (curr_link, new_front) -> if n==0 then d else
    (*If link is broken, call crawl with updated frontier and visited*)
    match CrawlerServices.get_page curr_link with
    | None -> let v = LinkSet.union visited (LinkSet.singleton curr_link) in
              crawl n new_front v d
    | Some p -> let v = LinkSet.union visited (LinkSet.singleton curr_link) in
    (*Create LinkSet of link list not including visited*)
    let insert_new_link r k =
      if LinkSet.member v k then r
      else LinkSet.insert k r  in
    let set_of_links =
      List.fold_left insert_new_link LinkSet.empty p.links in
    (*For each word, check if in dictionary and update record or insert new*)
    let rec update_dict (words:string list) (dct:WordDict.dict) =
      match words with
      | [] -> dct
      | k::t -> let new_dict =
                  match (WordDict.lookup dct k) with
                  | None -> WordDict.insert dct k (LinkSet.singleton curr_link)
                  | Some links -> WordDict.insert dct k
                    (LinkSet.union links (LinkSet.singleton curr_link))
                in
                update_dict t new_dict in
    crawl (n-1) (LinkSet.union set_of_links new_front) v (update_dict p.words d)

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty


(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
