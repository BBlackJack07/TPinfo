let rec puis p = match p with
	| 0 -> 1
	| _ when p mod 2 = 0 -> let n = puis (p / 2) in n * n
	| _ -> let n = puis (p / 2) in 2 * n * n
;;

let rec filtre_liste f = function
	| [] -> []
	| h::t when f h -> h :: filtre_liste f t
	| _::t -> filtre_liste f t
;;

filtre_liste (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

let filtre_table f t =
	let n = Array.length t in
	let rec filtre i = match i >= n with
		| true -> []
		| _ when f t.(i) -> i :: filtre (i + 1)
		| _ -> filtre (i + 1)
	in filtre 0
;;

filtre_table (fun x -> x mod 2 = 0) [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|];;

let nbre_compatible p n =
	if p mod 2 = 1 then false
	else begin
		let compteurs = Array.make 3 0 in
		let r = ref n in
		let k = ref 0 in
		let compatible = ref true in
		while !compatible && !k < p do
			let b = !r mod 2 in
			r := !r / 2;
			k := !k + 1;
			if b = 0 then begin
				compteurs.(0) <- compteurs.(0) + 1;
				compteurs.(1) <- compteurs.(1) + 1;
				compteurs.(2) <- 0
			end else begin
				compteurs.(1) <- 0;
				compteurs.(2) <- compteurs.(2) + 1
			end;
			compatible := compteurs.(2) < 3 && 
							  compteurs.(1) < 3 && 
							  compteurs.(0) <= p/2 && !compatible;
		done;
		compteurs.(0) = p / 2 && !compatible
	end
;;

nbre_compatible 4 5;;
nbre_compatible 6 5;;

let convert p n = 
	let t = Array.make p 0 in
	let r = ref n in
	for k = p-1 downto 0 do
		let b = !r mod 2 in
		r := !r / 2;
		t.(k) <- b
	done;
	t
;;

convert 6 5;;
convert 4 5;;

let constructions_suites p =
	let n = (puis p) - 1 in
	let rec l aux i = match i >= 0 with
		| false -> aux
		| _ -> l (i :: aux) (i - 1)
	in
	l [] (n-1)
		|> filtre_liste (nbre_compatible p)
		|> List.map (convert p) 
		|> Array.of_list
;;

constructions_suites 6;;

type takuzu = {
	t: int array array;
	s: int array array;
	l: int list array;
	c: int list array;
	mutable n: int
};;

let initialisation grille =
	let p = Array.length grille in
	let s = constructions_suites p in
	let f i u =
		let comp = ref true in
		for j = 0 to p - 1 do
			comp := (grille.(i).(j) = 2 || grille.(i).(j) = u.(j)) && !comp
		done;
		!comp
	and g i u = 
		let comp = ref true in
		for j = 0 to p - 1 do
			comp := (grille.(j).(i) = 2 || grille.(j).(i) = u.(j)) && !comp
		done;
		!comp
	in
	{t= grille;
	 s= s;
	 l= Array.init p (fun i -> filtre_table (f i) s);
	 c= Array.init p (fun i -> filtre_table (g i) s);
	 n= let k = ref 0 in (
	 	 for i = 0 to p - 1 do for j = 0 to p - 1 do
	 	 	if grille.(i).(j) = 2 then incr k
	 	 done done; !k);}
;;

let rec test tak i = function
	| [] -> 2 
	| [k] -> tak.s.(k).(i)
	| k1::k2::_ when tak.s.(k1).(i) <> tak.s.(k2).(i) ->
		2
	| k1::t -> test tak i t
;;

