type labyrinthe = {
  largeur : int;
  hauteur : int;
  murs_largeur : int;
  murs_hauteur : int;
  murs : int array array;
  depart : int*int;
  arrivee : int*int
}



let path = "test/maze_6x6.laby"

let suite l = 
  match l with 
  | []    -> []
  | e::ll -> ll 



let load_file file_path =
  let in_c = open_in file_path in
 
  let rec aux file =
    try
      let line = input_line file in
      line::aux file
    with
      End_of_file -> []  in
  aux in_c

let transforme c = 
  match c with 
  | '+' | '-' | '|' -> 0
  | ' ' -> 1
  | 'E' -> 2
  | 'S' -> 3
  | _   -> failwith"Caractère %s interdit" c

let verifie_caractere c numero_ligne indice ligne_max indice_max = 
  (*si la ligne est paire , si l indice dans la ligne est pair, on doit avoir un +  , sinon on doit avoir *)
  (*si on est sur la premiere ligne on verifie si on a un + ou un -*)
  if indice > indice_max then
    failwith"Ligne trop grande"
  else
    if numero_ligne = 0 || numero_ligne = ligne_max then
      if indice mod 2 = 0 then c = '+' else c = '-' 
    else
      (*si on est au début ou a la fin de la ligne on doit avoir un caractere fermant*)
      if indice == 0 || indice == indice_max then
        if numero_ligne mod 2  = 0 then c = '+' else c = '|'
      else
        (*sinon on verifie selon la parite de numero_ligne et indice*)
        match numero_ligne mod 2 , indice mod 2 with 
        | 0 , 0 ->  c == '+'
        | 0 , 1 ->  c == '-' || c == 'E' || c == 'S' || c == ' '
        | 1 , 0 ->  c == '|' || c == 'E' || c == 'S' || c == ' '
        | 1 , 1 ->  c == 'S' || c == 'E' || c == ' '
        | _ , _ -> failwith"Ya un probleme avec le modulo" (*ce cas ne sera jamais atteint, c'est juste pour eviter les warnings*) 

let printMur numero_ligne indice = 
  
  match numero_ligne mod 2 , indice mod 2 with
  | 0 , 0       -> Printf.printf"+"
  | 0 , 1       -> Printf.printf"-"
  | 1 , 0 | 1, 1-> Printf.printf"|"
  | _, _ -> failwith"Probleme avec le modulo" (* cette ligne ne se fait jamais, c'est pur eviter les warnings du pattern matching *)

let printCaractere (layout: int array array) numero_ligne indice = 
  let i1 = layout.(numero_ligne).(indice) in
  match i1 with 
  | 0 -> printMur numero_ligne indice
  | 1 -> Printf.printf" "
  | 2 -> Printf.printf"E"
  | 3 -> Printf.printf"S"
  | _ -> failwith"Valeur interdite dans le layout"

let  printLigne (layout : int array array) numero_ligne indice_max = 
  let rec sousPrintLigne (layout : int array array) numero_ligne indice indice_max =
    if indice > indice_max then (
      Printf.printf"\n"
    )
    else(
      printCaractere layout numero_ligne indice;
      sousPrintLigne layout numero_ligne (indice + 1 ) indice_max ;) in
      
  sousPrintLigne layout numero_ligne 0 indice_max

let rec sousAffiche layout numero_ligne ligne_max indice_max = 
  if numero_ligne > ligne_max then(
    Printf.printf"\n"
  )else(
    printLigne layout numero_ligne indice_max;
    sousAffiche layout (numero_ligne + 1) ligne_max indice_max
  )

let afficheLabyrinthe labyrinthe = 
  let indice_max = labyrinthe.murs_largeur - 1 in
  let ligne_max = labyrinthe.murs_hauteur - 1 in
  sousAffiche labyrinthe.murs 0 ligne_max indice_max;

  ()


let print_2d_array arr =
  Array.iter (fun row ->
    Array.iter (fun elem -> print_int elem; print_string " ") row;
    print_newline ()
  ) arr

let rec parcours_ligne ligne numero_ligne array indice indice_max= 
    
  let c = ligne.[indice] in
  array.(numero_ligne).(indice) <- transforme c;
  if indice < indice_max then
    parcours_ligne ligne numero_ligne array (indice+1) indice_max 
  else
    ()




let rec verifieLigne (ligne:string) numero_ligne indice ligne_max indice_max (depart: int*int) (arrivee:int*int) =
  if indice > indice_max then
    (depart, arrivee)
  else
    let c = ligne.[indice] in
    (* Vérifie si le caractère est au bon endroit *)
    if not(verifie_caractere c numero_ligne indice ligne_max indice_max) then
      failwith "Caractere au mauvais endroit"
    else
      if c == 'E' then
        if depart = (-1, -1) then
          let new_depart = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) indice_max indice_max new_depart arrivee
        else
          failwith "Plusieurs departs"
      else if c == 'S' then
        if arrivee = (-1, -1) then
          let new_arrivee = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) indice_max indice_max depart new_arrivee
        else
          failwith "Plusieurs arrivees"
      else
        verifieLigne ligne numero_ligne (indice + 1) indice_max indice_max depart arrivee



let rec sousVerifie (file:string list) largeur hauteur numero_ligne (depart: int*int) (arrivee:int*int)  = 
  match file with 
  | []    -> (depart, arrivee)
  | ligne::reste -> 
      let nouveau_depart, nouveau_arrivee = verifieLigne ligne numero_ligne 0 (hauteur - 1) (largeur - 1) depart arrivee in 
      sousVerifie reste largeur hauteur (numero_ligne + 1) nouveau_depart nouveau_arrivee

let verifie (file:string list) largeur hauteur = 
  sousVerifie file largeur hauteur 0 (-1, -1) (-1, -1)
  




let constructeur file_name = 
  let file = load_file file_name in 
  (* On définit la taille du labyrinthe*)
  let hauteur = List.length file in
  let largeur =
  match file with 
  | e::ll ->  String.length e
  | _     -> failwith"Y  a rien dans le labyrinthe"   in
  let depart, arrivee = verifie file largeur hauteur in

  let _ = Printf.printf"Depart : (%d, %d)\n" (fst depart) (snd depart) in
  let _ = Printf.printf"Arrivee : (%d, %d)\n" (fst arrivee) (snd arrivee) in


  (* on construit le labyrinthe*)
  let layout = Array.make_matrix hauteur largeur 0 in


  let rec parcours file numero_ligne array indice_max =
    match file with 
    | [] -> ()
    | e::ll -> parcours_ligne e numero_ligne array 0 indice_max;
              parcours ll (numero_ligne + 1) array indice_max; in

  parcours file 0 layout (largeur - 1);
  let l = (largeur - 1) / 2 in
  let h = (hauteur - 1) / 2 in
  { largeur = l;
  hauteur = h;
  murs_largeur = largeur;
  murs_hauteur = hauteur;
  murs = layout;
  depart = depart;
  arrivee = arrivee }

let print_line line =
  String.iter (fun c -> let s = transforme c in Printf.printf"%d " s) line;
  Printf.printf"\n"
  
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste
let file = load_file path





let printLaby labyrinthe = 
  Printf.printf "Hauteur : %d\n" labyrinthe.hauteur; 
  Printf.printf "Largeur : %d\n" labyrinthe.largeur;
  Printf.printf "Largeur des murs : %d\n" labyrinthe.murs_largeur;
  Printf.printf "Hauteur des murs : %d\n" labyrinthe.murs_hauteur;
  Printf.printf "Plan des murs : \n";
  print_2d_array labyrinthe.murs 
  



let labyrinthe1 = constructeur path
let () = Printf.printf"\n"
let () = printLaby labyrinthe1
let () =  afficheLabyrinthe labyrinthe1
let fin = Printf.printf"End\n"
