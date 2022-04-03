# use "topfind";;
# require "graphics";;
Graphics.open_graph "" ;;


type hanjie = {grille : int array array; lignes : int list array;  colonnes : int list array};;


let vide_ecran () =
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
    Graphics.set_color Graphics.black;;

let dessiner h = let n=Array.length h.lignes and p=Array.length h.colonnes in
    let pas=min ((Graphics.size_x())/(p+2)) ((Graphics.size_y())/(n+2)) in
    let hx = (Graphics.size_x()-p*pas)/2 and hy = (Graphics.size_y()-n*pas)/2 in    
    let x j= hx + j*pas in let y i=hy+(n-1-i)*pas in
    vide_ecran();
    for i=0 to n-1 do for j=0 to p-1 do
       Graphics.set_color (match h.grille.(i).(j) with
                            | 1 -> Graphics.black
                            | 2 -> Graphics.blue
                            | 0 -> Graphics.white);
       Graphics.fill_rect ((x j)+1) ((y i)+1) (pas-1) (pas-1);
       Graphics.set_color Graphics.black;
    done; done;;


let h1 = {grille = Array.make_matrix 4 4 2; lignes=[|[1];[1;2];[3];[1;1]|]; colonnes=[|[1];[1;2];[2];[3]|]};;

let h2 = {grille = Array.make_matrix 20 15 2;
lignes=[|[7];[2;2];[2;7;2];[2;2;2;2];[1;2;2;1];[1;2;2;1];[2;2;2;2];[2;7;2];[2;2];[4;3];[1;1];[1;2];[1;1];[1;2];[5;1];[1;2;2];[1;1];[1;2];[1;3;1];[4;3]|];
colonnes=[|[4];[2;2];[2;2;2;2;2];[1;4;2;1;2;1];[1;2;2;1;1;1];[1;1;1;1;2;2];[1;1;1;7;1];[1;1;1;2];[1;1;1;1;1;1;1;1;1];[1;1;1;11];[1;2;2;1];[1;4;1];[2;2;2];[2;2];[4]|]};;

let h3 = {grille = Array.make_matrix 20 15 2;
lignes=[|[3];[1;2];[2;2];[4;1];[5;1];[6;1];[2;6;1];[2;1;6;1];[1;1;6;1];[2;9];[5;1;1];[4;8];[6;4;1];[3;4;1];[4;3;1];[4;2;1];[3;3];[4;2];[12];[9]|];
colonnes=[|[2;3];[2;6;1];[1;7;2];[2;2;6;2];[1;2;1;1;3;2];[1;5;1;1;5];[1;7;1;4];[1;9;3];[1;6;9];[1;6;6;2];[1;5;4;1;2];[1;3;3;1;1];[2;1;1;1;2];[3;3;1];[1]|]};;

let h4 = {grille = Array.make_matrix 20 15 2;
lignes = [|[1;1;1];[1;1;1;1];[1;1;1;1;1];[2;5;2];[2;5;2];[2;3;2];[7];[3];[3;3];[2;5;2];[2;5;2];[2;7;2];[1;7;1];[1;2;3;2;1];[1;3;1];[1;1;1;1];
[1;5;1];[2;5;2];[2;3;2];[1;1]|];
colonnes = [|[3];[3;2;2];[2;2;2];[2;2;5];[2;1;3];[3;1;5;3];[2;5;6;3];[6;6;3];[2;5;6;3];[3;1;5;3];[2;1;3];[2;2;5];[2;2;2];[4;2;2];[3]|]};;

let h5 = {grille = Array.make_matrix 40 30 2;
lignes=[|[5];[3;3];[9];[10];[6];[5];[5];[5];[5];[5];[5];[5];[4];[4;5];[5;14];[23];[24];[21;2];[10;3;1];[10;1;3];[11;5;1];[14;1;4;1];[7;10;1];
[6;4;10;2];[7;2;11];[4;2;8];[3;2;5];[1;2;4];[3;4;2];[5;3;2];[5;2;2];[4;2;2];[5;5];[5;3];[5;3];[4;3];[4;2];[4;2;1];[5;5];[6;5]|];
colonnes=[|[1];[2;8];[2];[3];[5];[11;9;4];[1;8;10;4];[9;11;4];[8;12;3;4];[6;13;3;4];[14;7];[10;4;6];[5;3;2;7];[5;2;1;1;3];[6;1;1;1];[5;1;2;2];[5;2;2;1];[4;3;2;2];[4;3;2;2];[4;5;2;2];[4;8;4];[3;6;2;7];[2;6;10];[2;7;2;2];[10;3];[6;2];[1;3;1];[2;1];[1;2];[5]|]};;


let rec ajouter x = function
    | [] -> []
    | h::t -> (x::h) :: ajouter x t
;;

let rec crible l i a = match l with
    | [] -> []
    | arr::t when arr.(i) = a -> arr :: crible t i a
    | _::t -> crible t i a
;;

let rec pos l d n = match l with
    | [] -> [[]]
    | k::t when d+k > n -> []
    | k::t -> (ajouter d (pos t (d+k+1) n)) @ (pos l (d+1) n)
;;

let rec decoder l1 l2 n =
    let a = Array.make n 0 in
    let rec remplir la lb = match la,lb with
    | [],_ | _,[] -> ()
    | ha::ta, hb::tb -> 
        for i = hb to hb+ha-1 do
            a.(i) <- 1
        done;
        remplir ta tb
    in remplir l1 l2;
    a
;;

let possibilites l n = pos l 0 n |> List.map (fun x -> decoder l x n);;

let init h = 
    let n,p = Array.length h.grille, Array.length h.grille.(0) in
    let t_L = Array.make n [] in
    let t_C = Array.make p [] in
    for i = 0 to n-1 do
        t_L.(i) <- possibilites h.lignes.(i) p
    done;
    for i = 0 to p-1 do
        t_C.(i) <- possibilites h.colonnes.(i) n
    done;
    t_L,t_C
;;


let test l j =
    let rec equals = function
        | [] | [_] -> true
        | a::(b::_ as t) -> a.(j) = b.(j) && equals t
    in
    if l = [] then failwith "Pas de solution" else if not (equals l) then 2 else (List.hd l).(j)
;;

let test_lignes h t_L =
    let n,p = Array.length h.grille, Array.length h.grille.(0) in
    let modifies = ref [] in
    for i = 0 to n-1 do
        for j = 0 to p-1 do
            if h.grille.(i).(j) = 2 then
                let t = test t_L.(i) j in
                if t <> 2 then (h.grille.(i).(j) <- t; modifies := (i,j) :: !modifies)
        done
    done;
    !modifies
;;

let test_colonnes h t_C =
    let n,p = Array.length h.grille, Array.length h.grille.(0) in
    let modifies = ref [] in
    for i = 0 to n-1 do
        for j = 0 to p-1 do
            if h.grille.(i).(j) = 2 then
                let t = test t_C.(j) i in
                if t <> 2 then (h.grille.(i).(j) <- t; modifies := (i,j) :: !modifies)
        done
    done;
    !modifies
;;

let simplifie_colonnes h t_C l = let q = ref l in
    while !q <> [] do
        let i,j = List.hd !q in
        q := List.tl !q;
        t_C.(j) <- crible t_C.(j) i h.grille.(i).(j)
    done
;;

let simplifie_lignes h t_L l = let q = ref l in
    while !q <> [] do
        let i,j = List.hd !q in
        q := List.tl !q;
        t_L.(i) <- crible t_L.(i) j h.grille.(i).(j)
    done
;;


let resolution h =
    let was_mod = ref true in
    let t_L, t_C = init h in (* a *)
    while !was_mod do
        let ml = test_lignes h t_L in (* b *)
        simplifie_colonnes h t_C ml; (* c *)
        let mc = test_colonnes h t_C in (* d *)
        simplifie_lignes h t_L mc; (* e *)
        was_mod := mc <> [] || ml <> []
    done
;;

