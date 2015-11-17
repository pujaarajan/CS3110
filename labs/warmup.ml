open Async.Std

(* do some meaningless computation for about 5 seconds,
   depending on machine speed *)
let busywait =
  let ctr = ref 0 in fun () ->
  ctr := 0;
  for i = 1 to 500_000_000 do
    incr ctr
  done

(* helper function to print a string *)
let output s = Printf.printf "%s\n%!" s

let _ = output "A"
let d = return 42
let _ = output "B"
let _ = upon d (fun n -> output "C")
let _ = output "D"
let _ = busywait(); Scheduler.go()
