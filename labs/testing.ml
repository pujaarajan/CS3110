open Async.Std

let test_async_eq (d : 'a Deferred.t) (v : 'a) : bool =
  Thread_safe.block_on_async (fun () -> d) = Core.Std.Result.Ok v

let test_async_eq_with_timeout (d : 'a Deferred) (v : a) : bool =

let add x y =
  after (Core.Std.sec 2.) >>= fun () ->
  return (x + y)

TEST "check add" = test_async_eq (add 1 2) 3 = true

let broken x y =
  never () >>= fun () ->
  return (x + y)

TEST "check broken" = test_async_eq (broken 1 2) 3 = true


