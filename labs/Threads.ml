let t1 = Thread.create printf "Goodbye, OCaml"

let t1 = Thread.create printf "Goodbye, OCaml, from thread %n" % t1.id
let t2 = Thread.create printf "Goodbye, OCaml, from thread %n" % t2.id
let t3 = Thread.create printf "Goodbye, OCaml, from thread %n" % t3.id

let _ = Thread.join t1