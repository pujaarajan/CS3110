open Async.Std

(**
 * stdin represents input from the command line.
 * (Reader.read_line stdin) will return a deferred that becomes determined when
 * the user types in a line and presses enter.
 *)
let stdin : Reader.t = Lazy.force Reader.stdin


(* hypothetical synchronous code
let f () : unit =
  (* prompt the user *)
  printf "enter a line:\n";

  (* read the input *)
  let line = Reader.read_line stdin in

  (* wait 3 seconds *)
  let ()   = after (Core.Std.sec 3.) in

  (* print out "done" *)
  printf "done\n";

  (* exit the program *)
  exit 0
*)

(* actual asynchronous code *)
let f () : unit Deferred.t = failwith "TODO"
  (* prompt the user *)
  (* read the input *)
  (* wait 3 seconds *)
  (* print out "done" *)
  (* exit the program *)

  printf "enter a line:\n" >>=
  let line = Reader.read_line stdin >>=
  after (Core.Std.sec 3.) in]
  printf "done\n"; exit 0

let _ = f ()
let _ = Scheduler.go ()

