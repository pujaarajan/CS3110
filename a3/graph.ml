module type NODE =
sig
  type node

  (* Require that nodes be comparable for efficiency. *)
  val compare : node -> node -> Order.order
  val string_of_node : node -> string
  val gen : unit -> node
end

(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig
  module N : NODE
  type node = N.node
  type graph

  val empty : graph

  val nodes : graph -> node list

  val is_empty : graph -> bool

  val add_node : graph -> node -> graph

  (* Adds the nodes if they aren't already present. *)
  val add_edge : graph -> node -> node -> graph

  (* Return None if node isn't in the graph *)
  val neighbors : graph -> node -> node list option

  (* Return None if node isn't in the graph *)
  val outgoing_edges : graph -> node -> (node * node) list option

  val has_node : graph -> node -> bool

  (* Return None if the graph is empty *)
  val get_random_node : graph -> node option

  val string_of_graph : graph -> string
end

module Graph(NA: NODE) : (GRAPH with module N = NA) =
struct
  open Order
  module N = NA
  type node = N.node

  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor set
     Every node in the graph must be a key in the dictionary.
  *)

  module NeighborSet = Myset.Make(
     struct
        type t = node
        let compare = N.compare
        let string_of_t = N.string_of_node
        let gen = N.gen
        let gen_random = N.gen
        let gen_gt x () = N.gen ()
        let gen_lt x () = N.gen ()
        let gen_between x y () = None
      end)

  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = N.compare
      let string_of_key = N.string_of_node
      let string_of_value ns = NeighborSet.string_of_set ns
      let gen_key = N.gen
      let gen_key_random = N.gen
      let gen_key_gt x () = N.gen ()
      let gen_key_lt x () = N.gen ()
      let gen_key_between x y () = None
      let gen_value () = NeighborSet.empty
      let gen_pair () = (gen_key(),gen_value())
    end)

  module IntNode = Dict.Make(
    struct
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = N.string_of_node
      let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt x () = 1
      let gen_key_lt x () = (-1)
      let gen_key_between x y () = None
      let gen_value = N.gen
      let gen_pair () = (gen_key(),gen_value())
    end)

 type graph = { edges : EdgeDict.dict ;
                num_nodes : int ;
                index_to_node_map : IntNode.dict }

 let empty : graph = { edges = EdgeDict.empty;
                       num_nodes = 0;
                       index_to_node_map = IntNode.empty }

 let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n (NeighborSet.empty) ;
       num_nodes = g.num_nodes + 1 ;
       index_to_node_map =
         IntNode.insert g.index_to_node_map g.num_nodes n }

  let nodes g =
    EdgeDict.fold (fun k v r -> k :: r) [] g.edges

  let is_empty g = (g.num_nodes = 0)

  (* Adds the nodes if they aren't already present. *)
  let add_edge g src dst =
    let new_neighbors = match EdgeDict.lookup g.edges src with
      | None -> NeighborSet.insert dst NeighborSet.empty
      | Some s -> NeighborSet.insert dst s
    in
      (* ensure both src and dst in the graph before adding edge *)
    let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}

  let neighbors g n : node list option =
    match EdgeDict.lookup g.edges n with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun neigh r -> neigh :: r) [] s)

  let outgoing_edges g src : (node * node) list option =
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun dst r ->
                                             (src, dst) :: r) [] s)

  let has_node g n =
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

  let get_random_node g =
    if g.num_nodes = 0 then None else
      let r = Random.int (g.num_nodes) in
        IntNode.lookup g.index_to_node_map r

  let string_of_graph g =
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)
end

module NamedGraph =
struct
  include(Graph(struct
                  type node = string
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                end))
  let from_edges (es: (string * string) list) : graph =
    List.fold_left (fun g (src, dst) -> add_edge g src dst) empty es
end

(* Wrap our tests in a module so that they don't pollute the namespace *)
module TestGraph =
struct
  module G = NamedGraph

  let g = G.add_edge G.empty "a" "b"
  let g2 = G.add_edge g "a" "c"

  let deopt_len lo =
    match lo with
      | None -> 0
      | Some xs -> List.length xs

  let deopt_lst lo =
    match lo with
      | None -> []
      | Some xs -> xs

  let deopt_node no =
    match no with
      | None -> "None"
      | Some n -> n

  let _ = (
    assert (G.has_node g "a");
    assert (G.has_node g "b");
    assert (G.has_node g "c" = false);
    assert (G.has_node g2 "c");
    assert (G.has_node g2 "d" = false);

    assert (List.length (G.nodes G.empty) = 0) ;
    assert (List.length (G.nodes (G.add_node G.empty "a")) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (deopt_len (G.outgoing_edges g2 "a") = 2) ;
    assert (deopt_len (G.outgoing_edges g2 "b") = 0) ;
    assert (deopt_len (G.outgoing_edges g2 "c") = 0) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (deopt_len (G.neighbors g2 "b") = 0) ;
    assert (deopt_len (G.neighbors g2 "c") = 0) ;
    assert (G.neighbors g2 "d" = None) ;

    assert (let t = deopt_lst (G.neighbors g2 "a") in
              t = ["b";"c"] || t = ["c";"b"]) )
end
