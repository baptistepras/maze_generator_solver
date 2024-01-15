type labyrinthe = {
  largeur : int;
  hauteur : int;
  murs_largeur : int;
  murs_hauteur : int;
  murs : int array array;
  depart : int*int;
  arrivee : int*int
}

type direction = N | O | S | E


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
  | _  -> failwith"Caractère %s interdit" c

let verifie_caractere c numero_ligne indice ligne_max indice_max = 
  (*Si la ligne est paire , si l'indice dans la ligne est pair, on doit avoir un '+', sinon on doit avoir '|'*)
  (*Si l'on est sur la première ligne on vérifie si l'on a un '+' ou un '-'*)
  if indice > indice_max then
    failwith"Ligne trop grande"
  else
    if numero_ligne = 0 || numero_ligne = ligne_max then
      if indice mod 2 = 0 then c = '+' else c = '-' 
    else
      (*Si l'on est au début ou à la fin de la ligne on doit avoir un caractère fermant*)
      if indice = 0 || indice = indice_max then
        if numero_ligne mod 2 = 0 then c = '+' else c = '|'
      else
        (*Sinon on vérifie selon la parité de numero_ligne et indice*)
        match (numero_ligne mod 2), (indice mod 2) with 
        | 0 , 0 ->  c = '+'
        | 0 , 1 ->  c = '-' || c = 'E' || c = 'S' || c = ' '
        | 1 , 0 ->  c = '|' || c = 'E' || c = 'S' || c = ' '
        | 1 , 1 ->  c = 'S' || c = 'E' || c = ' '
        | _ , _ -> failwith"Problème de modulo" (*Ce cas ne sera jamais atteint, c'est juste pour éviter les warnings*) 




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
      failwith "Caractère au mauvais endroit"
    else
      if c = 'E' then
        if depart = (-1, -1) then
          let new_depart = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max new_depart arrivee
        else
          failwith "Plusieurs départs"
      else if c = 'S' then
        if arrivee = (-1, -1) then
          let new_arrivee = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max depart new_arrivee
        else
          failwith "Plusieurs arrivées"
      else
        verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max depart arrivee



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
  | _     -> failwith"Rien dans le labyrinthe"   in
  let depart, arrivee = verifie file largeur hauteur in

  (*let _ = Printf.printf"Depart : (%d, %d)\n" (fst depart) (snd depart) in
  let _ = Printf.printf"Arrivee : (%d, %d)\n" (fst arrivee) (snd arrivee) in*)


  (*0n construit le labyrinthe*)
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


let file = load_file path


let rec sousGenereLigneVide array numero_ligne  indice_max indice = 
  if indice > indice_max then
    ()
  else (
    array.(numero_ligne).(indice) <- 1;
    sousGenereLigneVide array numero_ligne indice_max (indice + 2) 

  )
let genereLigneVide array numero_ligne indice_max  = 
  sousGenereLigneVide array numero_ligne indice_max 1 

let rec sousGenereVide array numero_ligne ligne_max indice_max = 
  if numero_ligne >= ligne_max then
    ()
  else(
    genereLigneVide array numero_ligne indice_max;
    sousGenereVide array (numero_ligne + 2) ligne_max indice_max; )



let genereLabyVide hauteur largeur depart arrivee= 
  let largeur_murs  =  2 * largeur + 1 in
  let hauteurs_murs =  2 * hauteur + 1 in
  
   let lay = (Array.make_matrix hauteurs_murs  largeur_murs 0) in
    
    sousGenereVide lay 1 (hauteurs_murs-1) (largeur_murs -1);
  lay.(fst depart).(snd depart)   <- 2;
  lay.(fst arrivee).(snd arrivee) <- 3;


  {largeur = largeur;
   hauteur = hauteur;
   murs_largeur = largeur_murs;
   murs_hauteur = hauteurs_murs;
   murs = lay;
   depart = depart;
   arrivee = arrivee; 
  
  } 
 




let intDirection n =
  match n with
  | 1 -> N
  | 2 -> O
  | 3 -> S
  | 4 -> E
  | _  -> failwith"Mauvaises valeurs"

let directionsPossibles (visitees:bool array array) numero_ligne indice ligne_max indice_max =
    (*Regarde dans quelles cases on peut aller en fonction de la position de la case et des cases déjà visitées*)
  
    let positions = ref [] in

  if indice + 2 < indice_max   && not visitees.(numero_ligne).(indice + 2) then
    positions := E :: !positions
  else
    (*Gère le cas où la case à l'Est n'existe pas ou a déjà été visitée *)
    ();

  if indice - 2 > 0 && not visitees.(numero_ligne).(indice - 2) then
    positions := O :: !positions
  else
    (*Gère le cas où la case à l'Ouest n'existe pas ou a déjà été visitée *)
    ();
  
  if numero_ligne + 2 < ligne_max && not visitees.(numero_ligne+2).(indice) then
    positions := S :: !positions
  else
    (*Gère le cas où la case au Sud n'existe pas ou a déjà été visitée *)
    ();

  if numero_ligne - 2 > 0 && not visitees.(numero_ligne - 2).(indice) then
    positions := N :: !positions
  else
    (*Gère le cas où la case au Nord n'existe pas ou a déjà été visitée *)
    ();

  let positions_list = !positions  in
  positions_list










let rec generate_unique_random_numbers count range_max acc =
  if count <= 0 then acc
  else
    let num = Random.int range_max in
    if List.mem num acc then
      generate_unique_random_numbers count range_max acc
    else
      generate_unique_random_numbers (count - 1) range_max (num :: acc)

let random_numbers_without_repetition count range_max =
  generate_unique_random_numbers count range_max []





let movePosition numero_ligne indice direction = 
  match direction with
  | N -> (numero_ligne - 2, indice)
  | E -> (numero_ligne, indice + 2)
  | O -> (numero_ligne, indice - 2)
  | S -> (numero_ligne + 2, indice)


let ouvrirChemin (array:int array array) last_pos numero_ligne indice = 
  (*Ouvre le chemin entre une position selon une direction*)
  (*On supossera que l'on peut aller dans cette direction (condition verifiée avant)*)
  let y = ((fst last_pos) + numero_ligne) / 2 in
  let x = ((snd last_pos) + indice) / 2 in
  

  array.(y).(x) <- 1


let rec sousGenereLaby layout numero_ligne indice (visitees : bool array array) last_pos indice_max ligne_max = 
  if visitees.(numero_ligne).(indice) then
    ()
  else
    ouvrirChemin layout last_pos numero_ligne indice ; 
    visitees.(numero_ligne).(indice) <- true;
    let direc = directionsPossibles visitees numero_ligne indice ligne_max indice_max in
    let nombre_directions = List.length direc in
    if (nombre_directions) > 0 then
      let list_of_numbers = random_numbers_without_repetition nombre_directions nombre_directions in
      let direction = List.nth direc (List.hd list_of_numbers) in 

      
      let new_pos = movePosition numero_ligne indice direction in 
      
      (* let _ = Printf.printf"Old pos (%d %d) New pos (%d %d) " numero_ligne indice (fst new_pos) (snd new_pos)  in
      let _ = printDirection direction in
      let _ = Printf.printf"\n" in *)

      sousGenereLaby layout (fst new_pos ) (snd new_pos) visitees (numero_ligne, indice) indice_max ligne_max;

      (*On appellerécursivement pour les autres directions*)
      (*Si on ne peut aller qu'à un endroit, pas besoin de relancer des appels récursifs*)
      if nombre_directions > 1 then
        let s1 = List.tl list_of_numbers in
        let d1 = List.nth direc (List.hd s1) in
        let pos1 = movePosition numero_ligne indice d1 in
        sousGenereLaby layout (fst pos1) (snd pos1 ) visitees (numero_ligne, indice) indice_max ligne_max;
        if nombre_directions > 2 then
          let s2 = List.tl s1 in
          let d2 = List.nth direc (List.hd s2) in
          let pos2 = movePosition numero_ligne indice d2 in
          sousGenereLaby layout (fst pos2) (snd pos2 ) visitees  (numero_ligne, indice) indice_max ligne_max;
          if nombre_directions > 3 then
            let s3 = List.tl s2 in
            let d3 = List.nth direc (List.hd s3) in
            let pos3 = movePosition numero_ligne indice d3 in
            sousGenereLaby layout (fst pos3) (snd pos3 ) visitees  (numero_ligne, indice) indice_max ligne_max;
          else 
            ()
        else
          ()

      else
        ()


    else
        ()
      
  
let genereDepart hauteur largeur = 
  (2*(Random.int hauteur  )+ 1, 2*(Random.int largeur) + 1)

let rec genereArrivee depart hauteur largeur = 
  let x = 2*(Random.int largeur) + 1 in
  let y = 2*(Random.int hauteur) + 1 in
  if (y = (fst depart)) && (x = (snd depart)) then
    genereArrivee depart largeur hauteur 
  else
    (y,x)

let genereLabyrinthe hauteur largeur = 
  let dep = genereDepart hauteur largeur in
  let arr = genereArrivee dep hauteur largeur in



  let laby = genereLabyVide hauteur largeur dep arr in
  
  let visitees = Array.make_matrix laby.murs_hauteur  laby.murs_largeur  false in
  sousGenereLaby laby.murs (fst dep) (snd dep) visitees  dep (laby.murs_largeur - 1) (laby.murs_hauteur - 1) ;
  laby.murs.(fst dep).(snd dep) <- 2;
  laby.murs.(fst arr).(snd arr) <- 3;
  laby


let rec resolutionRec laby visit chemin case dir =
  (*dir permet de vérifier la présence d'un mur ou non entre l'ancienne case et la nouvelle, selon la direction prise*)
  if not (fst case >= 0 && fst case < laby.murs_hauteur && snd case >= 0 && snd case < laby.murs_largeur) || 
    visit.(fst case).(snd case) || laby.murs.(fst case + fst dir).(snd case + snd dir) = 0
    then ([], false)  (*Déplacement impossible = abandon*)
  else
    let chemin = (fst case + fst dir, snd case + snd dir)::case::chemin in  (*On sauvegarde la case courante dans le chemin*)
    let () = visit.(fst case).(snd case) <- true in (*On coche la case courante comme désormais visitée*)
    if case = laby.arrivee then
      (chemin, true)  (*On est arrivé, on renvoie le chemin*)
    else
      let chemin1 = resolutionRec laby visit chemin (fst case + 2, snd case) (-1, 0) in (*Sud*)
      let chemin2 = resolutionRec laby visit chemin (fst case - 2, snd case) (1, 0) in (*Nord*)
      let chemin3 = resolutionRec laby visit chemin (fst case, snd case + 2) (0, -1) in (*Est*)
      let chemin4 = resolutionRec laby visit chemin (fst case, snd case - 2) (0, 1) in (*Ouest*)
      if snd chemin1 then chemin1  (*On vérifie pour chaque chemin s'il a aboutit*)
      else if snd chemin2 then chemin2
      else if snd chemin3 then chemin3        
      else if snd chemin4 then chemin4
      else ([], false)

      
let resolution laby =
  (*On rappelle qu'une "salle" se trouve toujours sur un indice impair, un mur sur un indice pair*)
  let visitees = Array.make_matrix laby.murs_hauteur laby.murs_largeur false in
  visitees.(fst laby.depart).(snd laby.depart) <- false;
  let chemin = resolutionRec laby visitees [] laby.depart (0, 0) in (*Cherche le bon chemin*)
    let rec transformeChemin laby chemin = (*Remplace par des 4 dans le laby les cases du chemin*)
      match chemin with
      | e::ll -> if laby.murs.(fst e).(snd e) = 1 then (*Assure que l'on n'efface pas un mur pour y passer*)
        laby.murs.(fst e).(snd e) <- 4; transformeChemin laby ll
      | _ -> laby in
    transformeChemin laby (fst chemin)
 
(* let labyrinthe1 = constructeur path
let () = Printf.printf"\n"
let () = printLaby labyrinthe1
let () = Printf.printf"\n"

(* let () = ouvrirChemin a.murs 7 7 E
let () = ouvrirChemin a.murs 7 7 O
let () = ouvrirChemin a.murs 7 7 S
let () = ouvrirChemin a.murs 7 7 N
let () = afficheLabyrinthe a
 *)

let a = genereLabyrinthe 18 65
(* let a = genereLabyVide 20 40
 *)let () = Printf.printf"\n" 


 let () = afficheLabyrinthe a  


let fin = Printf.printf"End\n"
 *)
