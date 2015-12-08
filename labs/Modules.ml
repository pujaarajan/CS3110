module MyModule = struct
  let x = 3
  let f y = x + y
end

let a = MyModule.x;;
let b = MyModule.f 3;;