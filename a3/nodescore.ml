open Graph  (* Need the NODE sig *)

(* A NodeScore is a mapping from nodes to floating point scores.  It
 * adds a bit to the functionality of a dictionary. In particular, it
 * supports scaling and normalizing the score. *)
module type NODE_SCORE =
sig
  module N : NODE
  type node = N.node
  type node_score_map
  val empty : node_score_map
  val scale : node_score_map -> float -> node_score_map
    
  (* Scale the scores so that they add to 1.  If they originally add to zero,
   * leaves them unchanged *)
  val normalize : node_score_map -> node_score_map
  val nodes : node_score_map -> node list
  val get_score : node_score_map -> node -> float option

  (* Replace the old score, if there is one *)
  val set_score : node_score_map -> node -> float -> node_score_map

  (* Add to the old score.  (Use zero if no old score) *)
  val add_score : node_score_map -> node -> float -> node_score_map
    
  (* Create a zero valued node_score_map *)
  val zero_node_score_map : node list -> node_score_map

  (* Create a node_score_map with fixed values for each node *)
  val fixed_node_score_map : node list -> float -> node_score_map

  val string_of_node_score_map : node_score_map -> string
  val fold : (node -> float -> 'b -> 'b) -> 'b -> node_score_map -> 'b
end

module NodeScore(NA: NODE) : (NODE_SCORE with module N = NA) =
struct
  module N = NA
  type node = N.node

  module D = Dict.Make(
    struct
      type key = node
      type value = float
      let compare = N.compare
      let string_of_key = N.string_of_node
      let string_of_value = string_of_float
      let gen_key = N.gen
      let gen_key_lt x () = N.gen ()
      let gen_key_gt x () = N.gen ()
      let gen_key_random = N.gen
      let gen_key_between x y () = None
      let gen_value () = 0.0
      let gen_pair () = (gen_key(),gen_value())
    end)

  type node_score_map = D.dict
  let empty = D.empty
  let scale ns v =
    D.fold (fun n s r -> D.insert r n (v *. s)) D.empty ns
  let sum ns = 
    D.fold (fun n s r -> s +. r) 0.0 ns

  let normalize ns =
    let s = sum ns in
      if s = 0.0 then ns else scale ns (1.0 /. s)

  let nodes ns =
    D.fold (fun n s r -> n :: r) [] ns

  let get_score ns n = 
    D.lookup ns n

  let set_score ns n s =
    D.insert ns n s

  let add_score ns n s =
    match get_score ns n with
      | None -> set_score ns n s
      | Some v -> set_score ns n (s +. v)

  let string_of_node_score_map = D.string_of_dict

  let fold f u ns =
    D.fold f u ns
      
  let fixed_node_score_map nodes v =
    List.fold_left (fun r node -> set_score r node v) empty nodes

  let zero_node_score_map nodes = fixed_node_score_map nodes 0.0
end
