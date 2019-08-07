open List;;

exception NonValidArgument;;
exception MovimientoNoValido;;
exception NoSolucion;;

let comprobarTablero m n (x1,y1) (x2,y2)=
	(x1 >= 1 && m >= x1 &&
	n >= y1 && y1 >= 1 && 
	not((x1,y1)=(x2,y2)) &&
	m >= x2 && x2 >= 1 &&
	n >= y2 && y2 >= 1);;

let terminar actual fin l m n=
	(actual = fin) && ((length l) = m*n);;

let all_moves (x,y)= [(x+2,y-1);(x+1,y-2);(x-1,y-2);(x-2,y-1);(x-2,y+1);(x-1,y+2);(x+1,y+2);(x+2,y+1)];;

let dentroTablero m n (x,y)= x>=1 && y>=1 && x <= m && y <=n;;

let rec comprobarMovimiento m n mov l=
	(dentroTablero m n mov) && (not (mem mov l)) ;;

let rec movimientosPosibles m n mov todos= 
	let rec aux  m n allmovs posiblesMovs todos= match allmovs with
		| [] -> posiblesMovs
		| h::t ->
			if comprobarMovimiento m n h todos then
				aux m n t (h::posiblesMovs) todos
			else aux m n t posiblesMovs todos
	in aux m n (all_moves mov) [] todos;;
(*movimientos 6 6 (movimientosPosibles 6 6 (1,1) []) [(1,1)] (3,3);;*)
let rec movimientos m n listHijos (h2::t2) fin= match listHijos with
	| [] -> raise MovimientoNoValido
	| h::t -> 
		if terminar h fin (h::h2::t2) m n  then 
			h::h2::t2
		else 
		try
			movimientos m n (movimientosPosibles m n h (h2::t2)) (h::h2::t2) fin
		with
			MovimientoNoValido ->movimientos m n t (h2::t2) fin;;


let rec knight_tour m n init fin = 
	if comprobarTablero m n init fin then 
		List.rev (movimientos m n (movimientosPosibles m n init []) [init] fin)
	else raise NonValidArgument;;
	


(*Ejercicio 2*)
let rec anadirHijos h hijos ll= match hijos with
	| [] -> ll
	| h2::t -> anadirHijos h t ((h2::h)::ll);;


let rec shortest_knight_tour m n init fin=

	if comprobarTablero m n init fin then 
		let rec aux init l=
			match init with
			| [] -> raise NoSolucion
			| h::t -> 
				let li = movimientosPosibles m n (List.hd h) l in 
				if List.mem fin li then List.rev (fin::h)
			else aux (t@anadirHijos h li []) (li@l)
		in aux [[init]] [init]

	else raise NonValidArgument;;