type 'a foret = Vide | Node of 'a * 'a foret * 'a foret

let f = Node ( 2, Node (3, Vide, Node ( 7, Node (1, Vide, Node (6, Vide, Vide)), Node (49, Vide, Vide ))), 
                  Node (19, Node(15, Vide, Node (30, Node (17,Vide,Vide),Vide)), Vide) )

let t1 = Node (2, Node (3, Vide, Node ( 7, Node (1, Vide, Node (6, Vide, Vide)), Node (49, Vide, Vide ))), Vide);;
let t2 = Node (19, Node(15, Vide, Node (30, Node (17,Vide,Vide),Vide)), Vide);;

let rec longueur f = match f with
    | Vide -> 0
    | Node (_,_,f2) -> 1+longueur f2

let premier f = match f with
    | Vide -> failwith "foret vide"
    | Node (a,f1,_) -> Node (a,f1,Vide)

let fils t = match t with
    | Vide -> failwith "arbre vide"
    | Node (a, f1, _) -> f1

let rec taille f = match f with
    | Vide -> 0
    | Node(_,f1,f2) -> 1 + taille f1 + taille f2

let rec hauteur f = match f with
    | Vide -> -1
    | Node(_,f1,f2) -> max (1+hauteur f1) (hauteur f2)

let ajouter t f = match t with
    | Vide -> failwith "arbre vide"
    | Node (a,fils,Vide) -> Node (a, fils, f)
    | _ -> failwith "n'est pas un arbre"

let ordre t = longueur (fils t)

let fusion_arbre ta tb = match ta,tb with
    | Node(a,fa,Vide), Node(b,_,Vide) when a <= b -> Node (a, ajouter tb fa, Vide)
    | Node(_,_,Vide), Node (b,fb,Vide) -> Node (b, ajouter ta fb, Vide)
    | _ -> failwith "les arguments doivent etre des arbres binomiaux"

let rec bino = function
    | 0 -> Node (Random.int 10000, Vide, Vide)
    | i -> fusion_arbre (bino (i-1)) (bino (i-1))

let rec tas = function
    | [] -> Vide
    | n::q -> ajouter (bino n) (tas q)

let rec imprimer f = match f with
    | Vide -> print_char ' '
    | Node (a,t1,f1) -> print_int (ordre f); print_char ' '; imprimer f1

let rec union fa fb = match fa,fb with
    | f,Vide | Vide,f -> f
    | Node(a,ga,da), Node(b,_,_) when ordre fa <= ordre fb ->
        ajouter (Node (a,ga,Vide)) (union da fb)
    | Node(_,_,_), Node (b,gb,db) -> ajouter (Node (b,gb,Vide)) (union db fa)

let rec fusion_foret f = match f with
    | Vide | Node (_,_,Vide) -> f
    | Node (a,f1, (Node(b,f3,f4) as f2)) -> let t1 = Node (a,f1,Vide) and t2 = Node(b,f3,Vide) in
        if ordre t1 < ordre t2 then Node (a,f1,fusion_foret f2)
        else match f4 with
            | Vide -> fusion_arbre t1 t2
            | Node (c,f5,f6) when ordre (Node (c,f5,Vide)) = ordre t2 -> 
                ajouter t1 (fusion_foret (ajouter (fusion_arbre t2 (Node (c,f5,Vide))) f6))
            | _ -> fusion_foret (ajouter (fusion_arbre t1 t2) f4)


let fusion t1 t2 = fusion_foret (union t1 t2)

let insertion a f = fusion (Node (a,Vide,Vide)) f
