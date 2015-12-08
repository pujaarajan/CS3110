(*** Examples of using the standard library Map functor ***)

(* Example 1:  a map from Strings *)

(* We create a map that will have strings as keys by passing
   the String module to the Map.Make functor. *)

module StringMap = Map.Make(String)
    
(* Now we can use the resulting StringMap module to do things
   like map names to GPAs. *)
       
let sm = StringMap.(empty |> add "Alice" 4.0 |> add "Bob" 3.7)
let _  = assert (StringMap.find "Bob" sm = 3.7)
let _  = assert (not (StringMap.exists (fun k _ -> k="Cindy") sm))

(* Example 2:  a map from Ints *)

(* The Map.Make functor requires us to pass in a structure that
   provides a type t and a function compare.  The String module
   happened to already do that, but there is no Int module that
   we can use.  So, let's write our own... *)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module IntMap  = Map.Make(Int)

(* or, in one line... *)
module IntMap2 = Map.Make(struct type t = int let compare = Pervasives.compare end)
    
(* Now we can use the resulting IntMap module to do things
   like map ints to their English representation. *)  
    
let im = IntMap.(empty |> add 1 "one" |> add 2 "two")
let () = assert (IntMap.fold (fun k _ acc -> k+acc) im 0 = 3)

(* Example 3:  a map from records *)

type name = {first:string; last:string}

(* The next line is commented out for a reason; keep reading and
   you'll find out why. *)
(* type name = {last:string; first:string} *)
  
(* Here are some example names, and a function to print them. *)
 
let k1 = {last="Kardashian";first="Kourtney"}
let k2 = {last="Kardashian";first="Kimberly"}
let k3 = {last="Kardashian";first="Khloe"}
let k4 = {last="West";first="Kanye"}

let print_entry {first;last} v = Printf.printf "%s %s: %i\n" first last v

(* We first create a map over names that uses the standard library's built-in
   comparison function. *)

module NameMap1 = Map.Make(struct type t = name let compare = Pervasives.compare end)
let nm = NameMap1.(empty |> add k1 1979 |> add k2 1980 |> add k3 1984
                  |> add k4 1977)
                  
(* If you run the next line of code, you'll note how the default compare
   causes names to be printed in a particular sorted order:  by first name,
   then by last name.  
	   Kanye West: 1977
	   Khloe Kardashian: 1984
	   Kimberly Kardashian: 1980
	   Kourtney Kardashian: 1979  *)
                  
let () = NameMap1.iter print_entry nm

(* But what if we wanted to sort by last name, then by first name? 
   It turns out that if we defined the name type, above, in the 
   way that is commented out, the built-in comparison function would
   do what we want.  But that violates the general principle that
   the order of fields in a record shouldn't matter. *)

(* So here's a better way to achieve that sort order.  First,
   we define a module for names that provides a function that sorts in
   the order that we actually want. *)

module Name = struct
  type t = name
  let compare {first=first1;last=last1} {first=first2;last=last2} =
    match Pervasives.compare last1 last2 with
    | 0 -> Pervasives.compare first1 first2
    | c -> c
end

(* Now we can create a map using that module. *)

module NameMap2 = Map.Make(Name)
let nm = NameMap2.(empty |> add k1 1979 |> add k2 1980 |> add k3 1984
                  |> add k4 1977)

(* And the sort order will be what we want:
	  Khloe Kardashian: 1984
	  Kimberly Kardashian: 1980
	  Kourtney Kardashian: 1979
	  Kanye West: 1977 *)	  
                  
let () = NameMap2.iter print_entry nm
