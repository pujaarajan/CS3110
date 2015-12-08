type vector = float array

let normalize vector : unit =
	let squared = Array.iter (fun x -> x*x) vector in
		let sum = Array.fold (fun a b -> a + b) 0 vector in
			let square_root = sqrt sum in
				vector = Array.iter (fun x -> x/square_root) vector

				(*compile type error says getting int instead of unit*)