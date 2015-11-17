open Async.Std

let output s = Printf.printf "%s\n%!" s

let fc = Reader.file_contents Sys.argv.(1)
let uc = Deferred.bind fc (fun s -> return (String.uppercase s))
let _ = upon uc (fun s -> output s; ignore(exit 0))

let _ = Scheduler.go ()

